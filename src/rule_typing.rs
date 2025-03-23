// list of rules

use std::collections::{BTreeSet, HashMap};

use rustpython_ast::ExprSubscript;
use rustpython_parser::ast::{
    Expr, Ranged, Stmt, StmtAssign, StmtExpr, StmtFor, StmtIf, StmtWhile,
};

use crate::{
    astutils::serialise_subscript,
    type_inference::{
        ConcreteValue, FreshVariableGenerator, NodeTypeDB, Scheme, Type, TypeEnv, TypeVar,
    },
};

pub type Heuristic = HashMap<Type, f32>;
pub type RuleEnv = HashMap<String, Heuristic>;
type Rule = Vec<(Type, f32)>;
pub struct RuleInferrenceError {
    pub message: String,
}

pub type RuleInferrenceRes = Result<(), RuleInferrenceError>;

/**
 * The gist is, we pass down the name of the variable being inferred,
 * the type we expect (either a fresh typevar or another type),
 * and the current rules environment containing the list of heuristics
 * associated with a given variable name.
 *
 * This will be used on a funcdef body...
 */
pub fn infer_stmts_with_rules(
    rule_env: &mut RuleEnv,
    body: &[Stmt],
    type_db: &mut NodeTypeDB,
) -> RuleInferrenceRes {
    let mut rule_inferrer = RuleInferrer::new();
    for stmt in body {
        match stmt {
            Stmt::FunctionDef(funcdef) => {
                if let Err(e) = infer_stmts_with_rules(rule_env, &funcdef.body, type_db) {
                    return Err(e);
                }
            }
            Stmt::Assign(StmtAssign { targets, .. }) => {
                // get subscript value's string -> if it's a nested subscript,
                // we'll get something like x[][]
                // infer type and add to rule checker,
                // remove rule type db! i don't think we'll need it anymore

                // if the root is not a name, we just ignore (so some sort of
                // recursive check... astutils time...!!)

                if !targets[0].is_subscript_expr() {
                    continue;
                }

                let subscript = targets[0].as_subscript_expr().unwrap();
                let value_string = if let Some(val_subscript) = subscript.value.as_subscript_expr()
                {
                    serialise_subscript(val_subscript)
                } else if let Some(val_name) = subscript.value.as_name_expr() {
                    val_name.id.as_str().to_string()
                } else {
                    continue;
                };

                let slice_type = type_db
                    .get(&subscript.slice.range())
                    .cloned()
                    .unwrap_or_else(|| rule_inferrer.get_new_typevar());

                let subscript_type = type_db
                    .get(&subscript.range())
                    .cloned()
                    .unwrap_or_else(|| rule_inferrer.get_new_typevar());

                infer_expr_with_rules(rule_env, &targets[0], &mut rule_inferrer, type_db);

                match slice_type {
                    Type::ConcreteType(ConcreteValue::Int) => rule_inferrer
                        .apply_index_assign_rule(
                            &value_string,
                            &slice_type,
                            &subscript_type,
                            rule_env,
                        ),
                    _ => rule_inferrer.apply_key_rule(
                        &value_string,
                        &slice_type,
                        &subscript_type,
                        rule_env,
                    ),
                }

                if let Some(val_subscript) = subscript.value.as_subscript_expr() {
                    let types_to_be_removed =
                        Vec::from([Type::ConcreteType(ConcreteValue::Str), Type::Range]);
                    remove_types_from_subscript_heuristic(
                        &val_subscript,
                        rule_env,
                        &types_to_be_removed,
                    );
                }
            }
            Stmt::If(StmtIf {
                test, body, orelse, ..
            })
            | Stmt::While(StmtWhile {
                test, body, orelse, ..
            }) => {
                infer_expr_with_rules(rule_env, test, &mut rule_inferrer, type_db);
                let _ = infer_stmts_with_rules(rule_env, body, type_db);
                let _ = infer_stmts_with_rules(rule_env, orelse, type_db);
            }
            Stmt::For(StmtFor {
                target,
                iter,
                body,
                orelse,
                ..
            }) => {
                let _ = infer_stmts_with_rules(rule_env, body, type_db);
                let _ = infer_stmts_with_rules(rule_env, orelse, type_db);

                let iter_string = get_node_string(iter);
                if iter_string.is_empty() {
                    infer_expr_with_rules(rule_env, &iter, &mut rule_inferrer, type_db);
                    continue;
                }
                let key_type = rule_inferrer.get_new_typevar();
                let val_type = rule_inferrer.get_new_typevar();
                rule_inferrer.apply_for_in_stmt_rule(&iter_string, &key_type, &val_type, rule_env);
            }
            Stmt::Expr(StmtExpr { value, .. }) => {
                infer_expr_with_rules(rule_env, value, &mut rule_inferrer, type_db);
            }
            _ => {}
        }
    }
    Ok(())
}

fn infer_expr_with_rules(
    rule_env: &mut RuleEnv,
    expr: &Expr,
    rule_inferrer: &mut RuleInferrer,
    type_db: &mut NodeTypeDB,
) {
    match expr {
        Expr::Call(call) => {
            // check name of called fn
            if !call.func.is_attribute_expr() && !call.func.is_name_expr() {
                return;
            }

            if let Some(name) = call.func.as_name_expr() {
                let func_name = name.id.as_str();
                let typevar = rule_inferrer.get_new_typevar();
                if !func_name.eq("len") || call.args.len() == 0 {
                    // continue looking inside function calls
                    for arg in &call.args {
                        infer_expr_with_rules(rule_env, arg, rule_inferrer, type_db);
                    }
                    return;
                }

                let arg = call.args.get(0);
                if let Some(v) = arg {
                    let arg_name = get_node_string(v);
                    if arg_name.is_empty() {
                        return;
                    }
                    rule_inferrer.apply_len_rule(&arg_name, &typevar, rule_env);
                }
                return;
            }

            let attr = call.func.as_attribute_expr().unwrap();
            if !attr.value.is_name_expr() {
                return;
            }

            let value = &attr.value;
            let attr_name = attr.attr.as_str();

            let value_string = get_node_string(value);

            if value_string.is_empty() {
                return;
            }

            if attr_name.eq("clear") || attr_name.eq("copy") {
                let typevar = rule_inferrer.get_new_typevar();
                rule_inferrer.apply_list_operation_rule(&value_string, &typevar, rule_env);
            }

            for arg in &call.args {
                infer_expr_with_rules(rule_env, &arg, rule_inferrer, type_db);
            }
        }
        Expr::Subscript(subscript) => {
            // make sure to infer type of value too!
            infer_expr_with_rules(rule_env, &subscript.value, rule_inferrer, type_db);

            let value_string = get_node_string(&subscript.value);

            if value_string.is_empty() {
                return;
            }

            let slice_type = type_db
                .get(&subscript.slice.range())
                .cloned()
                .unwrap_or_else(|| rule_inferrer.get_new_typevar());

            let subscript_type = type_db
                .get(&subscript.range())
                .cloned()
                .unwrap_or_else(|| rule_inferrer.get_new_typevar());

            if let Type::ConcreteType(ConcreteValue::Int) = slice_type {
                rule_inferrer.apply_index_rule(&value_string, &subscript_type, rule_env);
            } else {
                rule_inferrer.apply_key_rule(&value_string, &slice_type, &subscript_type, rule_env);
            }

            if subscript.value.is_subscript_expr() {
                let types_to_be_removed =
                    Vec::from([Type::ConcreteType(ConcreteValue::Str), Type::Range]);
                remove_types_from_subscript_heuristic(&subscript, rule_env, &types_to_be_removed);
            }
        }
        Expr::BoolOp(boolop) => {
            for val in &boolop.values {
                infer_expr_with_rules(rule_env, val, rule_inferrer, type_db);
            }
        }
        Expr::Compare(comp) => {
            // TODO: Implement `i in x` rule => x should be iterable with elt type i
            // if operator == In(), apply in rule
            infer_expr_with_rules(rule_env, &comp.left, rule_inferrer, type_db);
            for comparator in &comp.comparators {
                infer_expr_with_rules(rule_env, &comparator, rule_inferrer, type_db);
            }
        }
        Expr::BinOp(binop) => {
            infer_expr_with_rules(rule_env, &binop.left, rule_inferrer, type_db);
            infer_expr_with_rules(rule_env, &binop.right, rule_inferrer, type_db);

            let left_type = type_db
                .get(&binop.left.range())
                .cloned()
                .unwrap_or_else(|| rule_inferrer.get_new_typevar());

            let right_type = type_db
                .get(&binop.right.range())
                .cloned()
                .unwrap_or_else(|| rule_inferrer.get_new_typevar());

            if let Type::TypeVar(..) = left_type {
                return;
            }

            if let Type::TypeVar(..) = right_type {
                return;
            }

            if left_type != Type::ConcreteType(ConcreteValue::Str)
                || right_type != Type::ConcreteType(ConcreteValue::Str)
            {
                if let Some(subscript) = binop.left.as_subscript_expr() {
                    remove_types_from_subscript_heuristic(
                        subscript,
                        rule_env,
                        &[Type::ConcreteType(ConcreteValue::Str)],
                    );
                } else {
                    remove_type_from_heuristic(
                        &binop.left,
                        rule_env,
                        &Type::ConcreteType(ConcreteValue::Str),
                    );
                }

                if let Some(subscript) = binop.right.as_subscript_expr() {
                    remove_types_from_subscript_heuristic(
                        subscript,
                        rule_env,
                        &[Type::ConcreteType(ConcreteValue::Str)],
                    );
                } else {
                    remove_type_from_heuristic(
                        &binop.right,
                        rule_env,
                        &Type::ConcreteType(ConcreteValue::Str),
                    );
                }
            }
        }
        _ => {}
    }
}

pub fn get_inferred_rule_types(rule_env: &mut RuleEnv) -> TypeEnv {
    let mut type_env = TypeEnv::new();
    for (name, heuristic) in rule_env {
        let most_likely_type = get_most_likely_type(heuristic);
        type_env.insert(
            name.clone(),
            Scheme {
                type_name: Box::new(most_likely_type),
                bounded_vars: BTreeSet::new(),
            },
        );
    }
    type_env
}

fn get_most_likely_type(heuristic: &Heuristic) -> Type {
    let mut sorted_heuristics = heuristic.iter().collect::<Vec<(_, _)>>();
    sorted_heuristics.sort_by(|(t1, v1), (t2, v2)| {
        v2.partial_cmp(v1)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then_with(|| type_order(t1).cmp(&type_order(t2)))
    });
    sorted_heuristics[0].0.clone() // get type from tuple form of Heuristic (Type, f32)
}

/**
 * Checks type order, making sure to prefer concrete types
 * over types defined with type variables defined as placeholders
 * in the rule inferrer.
 */
fn type_order(typ: &Type) -> i32 {
    match typ {
        Type::ConcreteType(ConcreteValue::Str) => 1,
        Type::List(elt_type) => {
            if !contains_rule_generated_typevar(elt_type) {
                2
            } else {
                3
            }
        }
        Type::Mapping(key_type, val_type) => {
            if !contains_rule_generated_typevar(key_type)
                && !contains_rule_generated_typevar(val_type)
            {
                3
            } else {
                4
            }
        }
        Type::Range => 5,
        _ => 0,
    }
}

fn contains_rule_generated_typevar(typ: &Type) -> bool {
    match typ {
        Type::TypeVar(TypeVar(name)) => is_rule_generated(name),
        Type::List(elt_type) => contains_rule_generated_typevar(elt_type),
        Type::Mapping(key_type, value_type) => {
            contains_rule_generated_typevar(key_type) || contains_rule_generated_typevar(value_type)
        }
        _ => false,
    }
}

fn is_rule_generated(name: &str) -> bool {
    name.starts_with("r") // TODO: Fix to be more general
}

/**
 * Helper to get the string for either a name or a subscript node.
 * We should ignore any other type of node.
 */
fn get_node_string(node: &Expr) -> String {
    match node {
        Expr::Subscript(subscript) => serialise_subscript(subscript),
        Expr::Name(name) => name.id.as_str().to_string(),
        _ => "".to_string(),
    }
}

/**
 * Helper to tank string type prediction on every level of the subscript.
 */
pub fn remove_types_from_subscript_heuristic(
    subscript: &ExprSubscript,
    rule_env: &mut RuleEnv,
    types: &[Type],
) {
    if !subscript.value.is_name_expr() && !subscript.value.is_subscript_expr() {
        return;
    }

    for typ in types {
        remove_type_from_heuristic(&subscript.value, rule_env, typ);
    }

    if subscript.value.is_name_expr() {
        return;
    }

    let val_as_subscript = subscript.value.as_subscript_expr().unwrap();
    remove_types_from_subscript_heuristic(val_as_subscript, rule_env, types);
}

/**
 * Used only when we want to tank a type prediction e.g. for multi dimensional
 * subscripts, we should remove the string and range predictions
 */
pub fn remove_type_from_heuristic(node: &Expr, rule_env: &mut RuleEnv, type_to_remove: &Type) {
    let node_name = get_node_string(node);

    if node_name.is_empty() {
        return;
    }

    let heuristic = rule_env.get_mut(&node_name);

    if let Some(h) = heuristic {
        if h.contains_key(&type_to_remove) {
            h.remove(&type_to_remove);
        }
    }
}

struct RuleInferrer {
    pub fresh_var_generator: FreshVariableGenerator,
}

impl RuleInferrer {
    pub fn new() -> Self {
        Self {
            fresh_var_generator: FreshVariableGenerator::new("r"), // make sure this is different from type_inferrer
        }
    }

    pub fn get_new_typevar(&mut self) -> Type {
        let typevar_name = self.fresh_var_generator.next();
        Type::TypeVar(TypeVar(typevar_name))
    }

    pub fn apply_for_in_stmt_rule(
        &mut self,
        node: &str,
        key_type: &Type,
        val_type: &Type,
        rule_env: &mut RuleEnv,
    ) {
        let rule = Vec::from([
            (Type::List(Box::new(val_type.clone())), 2.0),
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::Range, 2.0),
        ]);
        self.apply_rule(node, &rule, rule_env);
    }

    /**
     * Apply the list operation rule to the given variable.
     * This rule states that when we detect a call in the form
     * x.m(a), where m is a list operation, x is definitely a list.
     */
    pub fn apply_list_operation_rule(&mut self, node: &str, typ: &Type, rule_env: &mut RuleEnv) {
        let rule = Vec::from([(Type::List(Box::new(typ.clone())), 10.0)]);
        self.apply_rule(node, &rule, rule_env);
    }

    /**
     * Apply the len rule to the given variable.
     * This rule states that when we call len on a variable,
     * this means that it is either a string, a sequence, or a set.
     */
    pub fn apply_len_rule(&mut self, node: &str, typ: &Type, rule_env: &mut RuleEnv) {
        let rule = Vec::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::List(Box::new(typ.clone())), 2.0),
            (Type::Range, 2.0),
        ]);
        self.apply_rule(node, &rule, rule_env);
    }

    /**
     * Apply the index rule to the given variable.
     * This rule states that when we index a given variable with an integer,
     * this means that it is either a string or a sequence (sets cannot be indexed).
     * TODO: Add mapping type.
     */
    pub fn apply_index_rule(&mut self, node: &str, typ: &Type, rule_env: &mut RuleEnv) {
        let rule = Vec::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::List(Box::new(typ.clone())), 2.0),
            (Type::Range, 2.0),
        ]);
        self.apply_rule(node, &rule, rule_env);
    }

    /**
     * Only use when we get a subscript that doesn't use ints to index
     */
    pub fn apply_key_rule(
        &mut self,
        node: &str,
        key_typ: &Type,
        val_typ: &Type,
        rule_env: &mut RuleEnv,
    ) {
        let rule = Vec::from([(
            Type::Mapping(Box::new(key_typ.clone()), Box::new(val_typ.clone())),
            4.0,
        )]);
        self.apply_rule(node, &rule, rule_env);
    }

    /**
     * Apply the index-assign rule to the given variable.
     * This rule states that when we index a given variable with an integer and we assign
     * a value to this slice, the variable is a list (all other sequence types are immutable).
     */
    pub fn apply_index_assign_rule(
        &mut self,
        node: &str,
        key_type: &Type,
        val_type: &Type,
        rule_env: &mut RuleEnv,
    ) {
        let rule = Vec::from([
            (Type::List(Box::new(val_type.clone())), 3.0),
            (
                Type::Mapping(Box::new(key_type.clone()), Box::new(val_type.clone())),
                2.0,
            ),
        ]);
        self.apply_rule(node, &rule, rule_env);
    }

    pub fn apply_rule(&mut self, node: &str, rule: &Rule, rule_env: &mut RuleEnv) {
        self.apply_rule_to_rule_env(node, rule, rule_env);
    }

    pub fn apply_rule_to_rule_env(&mut self, node_id: &str, rule: &Rule, rule_env: &mut RuleEnv) {
        let mut heuristics = match rule_env.get(node_id) {
            None => HashMap::new(),
            Some(map) => map.clone(),
        };
        for (typ, add_val) in rule {
            let val = match heuristics.get(&typ) {
                None => 0.0 as f32,
                Some(f) => *f,
            };
            heuristics.insert(typ.clone(), val + add_val);
        }
        rule_env.insert(node_id.to_string(), heuristics);
    }
}

#[cfg(test)]
mod tests;
