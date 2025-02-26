// list of rules

use std::collections::{BTreeSet, HashMap};

use rustpython_parser::{
    ast::{Expr, Ranged, Stmt, StmtAssign, StmtExpr, StmtFor, StmtIf, StmtWhile},
    text_size::TextRange,
};

use crate::type_inference::{
    ConcreteValue, FreshVariableGenerator, NodeTypeDB, Scheme, Type, TypeEnv, TypeVar,
};

pub type Heuristic = HashMap<Type, f32>;

pub type RuleEnv = HashMap<String, Heuristic>;

pub type RuleTypeDB = HashMap<TextRange, Heuristic>;

pub struct RuleInferrenceError {
    pub message: String,
}

pub type RuleInferrenceRes = Result<(), RuleInferrenceError>;

type Rule = Vec<(Type, f32)>;

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
    rule_type_db: &mut RuleTypeDB,
    type_db: &mut NodeTypeDB,
) -> RuleInferrenceRes {
    let mut rule_inferrer = RuleInferrer::new();
    for stmt in body {
        match stmt {
            Stmt::FunctionDef(funcdef) => {
                if let Err(e) =
                    infer_stmts_with_rules(rule_env, &funcdef.body, rule_type_db, type_db)
                {
                    return Err(e);
                }
            }
            Stmt::Assign(StmtAssign { targets, .. }) => {
                if !targets[0].is_subscript_expr() {
                    continue;
                }

                let subscript = targets[0].as_subscript_expr().unwrap();

                infer_expr_with_rules(
                    rule_env,
                    &targets[0],
                    rule_type_db,
                    &mut rule_inferrer,
                    type_db,
                );

                let subscript_value_range = subscript.value.range();
                match type_db.get(&subscript_value_range) {
                    Some(Type::Mapping(key, value)) => {
                        if let Type::ConcreteType(ConcreteValue::Int) = **key {
                            let inferred_subscript_type = if let Some(heuristic) =
                                rule_type_db.get(&subscript_value_range)
                            {
                                &get_most_likely_type(heuristic)
                            } else {
                                type_db
                                    .get(&subscript_value_range)
                                    .expect("All subscript types should be inferred in type inferrence step.")
                            };
                            let unified_type = rule_unify_types(&value, &inferred_subscript_type);

                            rule_inferrer.apply_index_assign_rule(
                                &subscript.value,
                                &unified_type,
                                rule_env,
                                rule_type_db,
                            );
                        } else {
                            rule_inferrer.apply_key_rule(
                                &subscript.value,
                                key,
                                value,
                                rule_env,
                                rule_type_db,
                            );
                        }
                    }
                    None => {
                        let typ = rule_inferrer.get_new_typevar();
                        rule_inferrer.apply_index_assign_rule(
                            &subscript.value,
                            &typ,
                            rule_env,
                            rule_type_db,
                        );
                    }
                    _ => {
                        let typ = rule_inferrer.get_new_typevar();
                        rule_inferrer.apply_index_assign_rule(
                            &subscript.value,
                            &typ,
                            rule_env,
                            rule_type_db,
                        );
                    }
                }

                if subscript.value.is_subscript_expr() {
                    remove_str_from_heuristic(&subscript.value, rule_env, rule_type_db);
                }
            }
            Stmt::If(StmtIf {
                test, body, orelse, ..
            })
            | Stmt::While(StmtWhile {
                test, body, orelse, ..
            }) => {
                infer_expr_with_rules(rule_env, test, rule_type_db, &mut rule_inferrer, type_db);
                let _ = infer_stmts_with_rules(rule_env, body, rule_type_db, type_db);
                let _ = infer_stmts_with_rules(rule_env, orelse, rule_type_db, type_db);
            }
            Stmt::For(StmtFor {
                target,
                iter,
                body,
                orelse,
                ..
            }) => {
                // TODO: Implement for-in loop rule on iter
                if let Some(Type::Mapping(key_type, val_type)) = type_db.get(&iter.range()) {
                    rule_inferrer.apply_for_in_stmt_rule(
                        &iter,
                        key_type,
                        val_type,
                        rule_env,
                        rule_type_db,
                    );
                } else {
                    let key_type = rule_inferrer.get_new_typevar();
                    let val_type = rule_inferrer.get_new_typevar();
                    rule_inferrer.apply_for_in_stmt_rule(
                        &iter,
                        &key_type,
                        &val_type,
                        rule_env,
                        rule_type_db,
                    );
                }
                let _ = infer_stmts_with_rules(rule_env, body, rule_type_db, type_db);
                let _ = infer_stmts_with_rules(rule_env, orelse, rule_type_db, type_db);
            }
            Stmt::Expr(StmtExpr { value, .. }) => {
                infer_expr_with_rules(rule_env, value, rule_type_db, &mut rule_inferrer, type_db);
            }
            _ => {}
        }
    }
    Ok(())
}

fn infer_expr_with_rules(
    rule_env: &mut RuleEnv,
    expr: &Expr,
    rule_type_db: &mut RuleTypeDB,
    rule_inferrer: &mut RuleInferrer,
    type_db: &mut NodeTypeDB,
) {
    match expr {
        Expr::Call(call) => {
            // check name of called fn
            if !call.func.is_attribute_expr() && !call.func.is_name_expr() {
                return;
            }

            if let Some(attr) = call.func.as_attribute_expr() {
                if !attr.value.is_name_expr() {
                    return;
                }

                let value = &attr.value;
                let attr_name = attr.attr.as_str();

                if attr_name.eq("clear") || attr_name.eq("copy") {
                    let typevar = rule_inferrer.get_new_typevar();
                    rule_inferrer.apply_list_operation_rule(
                        value,
                        &typevar,
                        rule_env,
                        rule_type_db,
                    );
                }

                return;
            }

            let func_name = call.func.as_name_expr().unwrap().id.as_str();
            let typevar = rule_inferrer.get_new_typevar();
            if func_name.eq("len") && call.args.len() == 1 {
                let arg = call.args.get(0);
                if let Some(v) = arg {
                    rule_inferrer.apply_len_rule(&v, &typevar, rule_env, rule_type_db);
                }
            }
        }
        Expr::Subscript(subscript) => {
            // infer type of value
            let value = &*subscript.value;
            infer_expr_with_rules(rule_env, value, rule_type_db, rule_inferrer, type_db);

            let typ = if let Some(heuristic) = rule_type_db.get(&value.range()) {
                &get_most_likely_type(heuristic)
            } else {
                let typ = type_db
                    .get(&value.range())
                    .expect("All subscript types should be inferred in type inferrence step.");
                match typ {
                    Type::Mapping(key, value)
                        if **key == Type::ConcreteType(ConcreteValue::Int) =>
                    {
                        &rule_unify_types(typ, &Type::List(value.clone()))
                    }
                    _ => &typ,
                }
            };

            if let Type::Mapping(key_type, val_type) = typ {
                if let Type::ConcreteType(ConcreteValue::Int) = **key_type {
                    match **val_type {
                        Type::ConcreteType(ConcreteValue::Str) | Type::TypeVar(..) | Type::Any => {
                            rule_inferrer.apply_index_rule(value, &val_type, rule_env, rule_type_db)
                        }
                        Type::ConcreteType(ConcreteValue::Int) => rule_inferrer
                            .apply_index_with_int_value(value, &val_type, rule_env, rule_type_db),
                        _ => rule_inferrer.apply_index_with_non_int_or_str_value(
                            value,
                            &val_type,
                            rule_env,
                            rule_type_db,
                        ),
                    }
                } else {
                    rule_inferrer.apply_key_rule(value, key_type, val_type, rule_env, rule_type_db);
                }
            } else {
                let val_type = rule_inferrer.get_new_typevar();
                rule_inferrer.apply_index_rule(value, &val_type, rule_env, rule_type_db);
            }
        }
        Expr::Compare(comp) => {
            // TODO: Implement `i in x` rule => x should be iterable with elt type i
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

pub fn get_inferred_rule_type_db(rule_type_db: &mut RuleTypeDB) -> NodeTypeDB {
    let mut type_db = NodeTypeDB::new();
    for (range, heuristic) in rule_type_db {
        let most_likely_type = get_most_likely_type(heuristic);
        type_db.insert(range.clone(), most_likely_type);
    }
    type_db
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

fn type_order(typ: &Type) -> i32 {
    match typ {
        Type::Mapping(..) => 1,
        Type::ConcreteType(ConcreteValue::Str) => 2,
        Type::List(..) => 3,
        Type::Range => 4,
        _ => 0,
    }
}

/**
 * A special variation of unification where we unify types to
 * resolve rule types. Returns the unified type rather than a substitution.
 */
pub fn rule_unify_types(expected: &Type, inferred: &Type) -> Type {
    match (expected, inferred) {
        (Type::Mapping(k1, v1), Type::Mapping(k2, v2)) => {
            let key = rule_unify_types(k1, k2);
            let value = rule_unify_types(v1, v2);
            Type::Mapping(Box::new(key), Box::new(value))
        }
        (Type::List(t1), Type::List(t2)) => Type::List(Box::new(rule_unify_types(t1, t2))),
        (Type::Mapping(key, val), Type::List(elt)) | (Type::List(elt), Type::Mapping(key, val))
            if **key == Type::ConcreteType(ConcreteValue::Int) =>
        {
            Type::List(Box::new(rule_unify_types(elt, val)))
        }
        (Type::Mapping(key, val), Type::ConcreteType(ConcreteValue::Str))
        | (Type::ConcreteType(ConcreteValue::Str), Type::Mapping(key, val))
            if **key == Type::ConcreteType(ConcreteValue::Int) =>
        {
            Type::List(Box::new(rule_unify_types(
                &Type::ConcreteType(ConcreteValue::Str),
                val,
            )))
        }
        (Type::Mapping(key, val), Type::Range) | (Type::Range, Type::Mapping(key, val))
            if **key == Type::ConcreteType(ConcreteValue::Int) =>
        {
            Type::List(Box::new(rule_unify_types(
                &Type::ConcreteType(ConcreteValue::Int),
                val,
            )))
        }
        (Type::TypeVar(..), typ) | (typ, Type::TypeVar(..)) => typ.clone(),
        (Type::ConcreteType(t1), Type::ConcreteType(t2)) if t1 != t2 => {
            panic!("Cannot unify incompatible types: {:?} and {:?}", t1, t2);
        }
        _ => expected.clone(),
    }
}

/**
 * Used only when we want to tank the string type prediction
 */
pub fn remove_str_from_heuristic(
    node: &Expr,
    rule_env: &mut RuleEnv,
    rule_type_db: &mut RuleTypeDB,
) {
    let string_type = Type::ConcreteType(ConcreteValue::Str);
    if let Some(name) = node.as_name_expr() {
        let id = name.id.as_str();
        let heuristic = rule_env.get_mut(id).unwrap();
        if heuristic.contains_key(&string_type) {
            heuristic.remove(&string_type);
        }
    }

    if let Some(subscript) = node.as_subscript_expr() {
        let heuristic = rule_type_db.get_mut(&subscript.range()).unwrap();
        if heuristic.contains_key(&string_type) {
            heuristic.remove(&string_type);
        }
        // recurse to remove string from all parent levels of the subscript
        remove_str_from_heuristic(&subscript.value, rule_env, rule_type_db);
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
        node: &Expr,
        key_type: &Type,
        val_type: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([
            (Type::List(Box::new(val_type.clone())), 2.0),
            (Type::Set(Box::new(val_type.clone())), 2.0),
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::Range, 2.0),
            (
                Type::Mapping(Box::new(key_type.clone()), Box::new(val_type.clone())),
                2.0,
            ),
        ]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    /**
     * Apply the list operation rule to the given variable.
     * This rule states that when we detect a call in the form
     * x.m(a), where m is a list operation, x is definitely a list.
     */
    pub fn apply_list_operation_rule(
        &mut self,
        node: &Expr,
        typ: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([(Type::List(Box::new(typ.clone())), 10.0)]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    /**
     * Apply the len rule to the given variable.
     * This rule states that when we call len on a variable,
     * this means that it is either a string, a sequence, or a set.
     */
    pub fn apply_len_rule(
        &mut self,
        node: &Expr,
        typ: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::List(Box::new(typ.clone())), 2.0),
            (Type::Set(Box::new(typ.clone())), 2.0),
            (Type::Range, 2.0),
        ]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    /**
     * Side effect of indexing a sequence with a value that is definitely
     * not an integer or a string.
     */
    pub fn apply_index_with_non_int_or_str_value(
        &mut self,
        node: &Expr,
        typ: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([(Type::List(Box::new(typ.clone())), 2.0)]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    /**
     * Side effect of indexing a sequence with a known int value.
     */
    pub fn apply_index_with_int_value(
        &mut self,
        node: &Expr,
        typ: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([(Type::List(Box::new(typ.clone())), 2.0), (Type::Range, 2.0)]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    /**
     * Apply the index rule to the given variable.
     * This rule states that when we index a given variable with an integer,
     * this means that it is either a string or a sequence (sets cannot be indexed).
     * TODO: Add mapping type.
     */
    pub fn apply_index_rule(
        &mut self,
        node: &Expr,
        typ: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::List(Box::new(typ.clone())), 2.0),
            (Type::Range, 2.0),
        ]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    /**
     * Only use when we get a subscript that doesn't use ints to index
     */
    pub fn apply_key_rule(
        &mut self,
        node: &Expr,
        key_typ: &Type,
        val_typ: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([(
            Type::Mapping(Box::new(key_typ.clone()), Box::new(val_typ.clone())),
            4.0,
        )]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    /**
     * Apply the index-assign rule to the given variable.
     * This rule states that when we index a given variable with an integer and we assign
     * a value to this slice, the variable is a list (all other sequence types are immutable).
     */
    pub fn apply_index_assign_rule(
        &mut self,
        node: &Expr,
        typ: &Type,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let rule = Vec::from([(Type::List(Box::new(typ.clone())), 3.0)]);
        self.apply_rule(node, &rule, rule_env, rule_type_db);
    }

    pub fn apply_rule(
        &mut self,
        node: &Expr,
        rule: &Rule,
        rule_env: &mut RuleEnv,
        rule_type_db: &mut RuleTypeDB,
    ) {
        if let Some(name) = node.as_name_expr() {
            let node_id = name.id.as_str();
            self.apply_rule_to_rule_env(node_id, rule, rule_env);
        } else {
            self.apply_rule_to_rule_type_db(&node.range(), rule, rule_type_db);
        }
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

    pub fn apply_rule_to_rule_type_db(
        &mut self,
        node_range: &TextRange,
        rule: &Rule,
        rule_type_db: &mut RuleTypeDB,
    ) {
        let mut heuristics = match rule_type_db.get(node_range) {
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
        rule_type_db.insert(node_range.clone(), heuristics);
    }
}

#[cfg(test)]
mod tests;
