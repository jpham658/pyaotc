// list of rules

use std::collections::{BTreeSet, HashMap};

use rustpython_parser::ast::{Expr, Ranged, Stmt, StmtAssign, StmtExpr};

use crate::type_inference::{
    ConcreteValue, FreshVariableGenerator, NodeTypeDB, Scheme, Type, TypeEnv, TypeVar,
};

pub type Heuristic = HashMap<Type, f32>;

pub type RuleEnv = HashMap<String, Heuristic>;

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
    rules_env: &mut RuleEnv,
    body: &[Stmt],
    type_db: &mut NodeTypeDB,
) -> RuleInferrenceRes {
    let mut rule_inferrer = RuleInferrer::new();
    for stmt in body {
        match stmt {
            Stmt::FunctionDef(funcdef) => {
                if let Err(e) = infer_stmts_with_rules(rules_env, &funcdef.body, type_db) {
                    return Err(e);
                }
            }
            Stmt::Assign(StmtAssign { targets, .. }) => {
                if targets[0].is_subscript_expr() {
                    let subscript = targets[0].as_subscript_expr().unwrap();
                    let target = &subscript.value;
                    if target.is_name_expr() {
                        let slice = *subscript.slice.clone();
                        let slice_type = match type_db.get(&slice.range()) {
                            Some(t) => t,
                            None => {
                                return Err(RuleInferrenceError {
                                    message: format!("Slice is not defined."),
                                });
                            }
                        };
                        let rhs_typ = match type_db.get(&subscript.range()) {
                            Some(t) => t,
                            _ => &rule_inferrer.get_new_typevar(),
                        };
                        let target_id = target.as_name_expr().unwrap().id.as_str();
                        match slice_type {
                            Type::Scheme(Scheme { type_name, .. }) => {
                                if let Type::ConcreteType(ConcreteValue::Int) = **type_name {
                                    rule_inferrer
                                        .apply_index_assign_rule(&target_id, rhs_typ, rules_env)
                                } else {
                                    rule_inferrer
                                        .apply_key_rule(&target_id, slice_type, rhs_typ, rules_env)
                                }
                            }
                            Type::ConcreteType(ConcreteValue::Int) => rule_inferrer
                                .apply_index_assign_rule(&target_id, rhs_typ, rules_env),
                            _ => rule_inferrer
                                .apply_key_rule(&target_id, slice_type, rhs_typ, rules_env),
                        }
                    }
                }
            }
            Stmt::Expr(StmtExpr { value, .. }) => {
                infer_expr_with_rules(rules_env, value, &mut rule_inferrer, type_db);
            }
            _ => {}
        }
    }
    Ok(())
}

fn infer_expr_with_rules(
    rules_env: &mut RuleEnv,
    expr: &Expr,
    rule_inferrer: &mut RuleInferrer,
    type_db: &NodeTypeDB,
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

                let value = attr.value.as_name_expr().unwrap();
                let attr_name = attr.attr.as_str();

                if attr_name.eq("append") {
                    let arg_type = type_db
                        .get(&call.args[0].range())
                        .expect("Node not defined in type db...");
                    rule_inferrer.apply_list_operation_rule(
                        &value.id.as_str(),
                        arg_type,
                        rules_env,
                    );
                } else if attr_name.eq("clear") || attr_name.eq("copy") {
                    let typevar = rule_inferrer.get_new_typevar();
                    rule_inferrer.apply_list_operation_rule(
                        &value.id.as_str(),
                        &typevar,
                        rules_env,
                    );
                }

                return;
            }

            let func_name = call.func.as_name_expr().unwrap().id.as_str();
            let typevar = rule_inferrer.get_new_typevar();
            if func_name.eq("len") && call.args.len() == 1 {
                let arg = call.args.get(0);
                if let Some(v) = arg {
                    if v.is_name_expr() {
                        rule_inferrer.apply_len_rule(
                            &v.as_name_expr().unwrap().id,
                            &typevar,
                            rules_env,
                        );
                    }
                }
            }
        }
        Expr::Subscript(subscript) => {
            // infer type of &subscript.slice
            // We only use rule inference when the value is a name
            let value = &*subscript.value;
            let typevar = rule_inferrer.get_new_typevar();
            if value.is_name_expr() {
                rule_inferrer.apply_index_rule(
                    &value.as_name_expr().unwrap().id.as_str(),
                    &typevar,
                    rules_env,
                );
            }
        }
        _ => {}
    }
}

pub fn get_inferred_rule_types(rules_env: &mut RuleEnv) -> TypeEnv {
    let mut type_env = TypeEnv::new();
    for (name, heuristic) in rules_env {
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

fn type_order(typ: &Type) -> i32 {
    match typ {
        Type::Mapping(..) => 1,
        Type::ConcreteType(ConcreteValue::Str) => 2,
        Type::List(..) => 3,
        Type::Range => 4,
        _ => 0,
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

    /**
     * Apply the list operation rule to the given variable.
     * This rule states that when we detect a call in the form
     * x.m(a), where m is a list operation, x is definitely a list.
     */
    pub fn apply_list_operation_rule(
        &mut self,
        node_id: &str,
        typ: &Type,
        rules_env: &mut RuleEnv,
    ) {
        let rule = Vec::from([(Type::List(Box::new(typ.clone())), 10.0)]);
        self.apply_rule(node_id, &rule, rules_env);
    }

    /**
     * Apply the len rule to the given variable.
     * This rule states that when we call len on a variable,
     * this means that it is either a string, a sequence, or a set.
     */
    pub fn apply_len_rule(&mut self, node_id: &str, typ: &Type, rules_env: &mut RuleEnv) {
        let rule = Vec::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::List(Box::new(typ.clone())), 2.0),
            (Type::Set(Box::new(typ.clone())), 2.0),
            (Type::Range, 2.0),
        ]);
        self.apply_rule(node_id, &rule, rules_env);
    }

    /**
     * Apply the index rule to the given variable.
     * This rule states that when we index a given variable with an integer,
     * this means that it is either a string or a sequence (sets cannot be indexed).
     * TODO: Add mapping type.
     */
    pub fn apply_index_rule(&mut self, node_id: &str, typ: &Type, rules_env: &mut RuleEnv) {
        let rule = Vec::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::List(Box::new(typ.clone())), 2.0),
            (Type::Range, 2.0),
        ]);
        self.apply_rule(node_id, &rule, rules_env);
    }

    /**
     * Only use when we get a subscript that doesn't use ints to index
     */
    pub fn apply_key_rule(
        &mut self,
        node_id: &str,
        key_typ: &Type,
        val_typ: &Type,
        rules_env: &mut RuleEnv,
    ) {
        let rule = Vec::from([(
            Type::Mapping(Box::new(key_typ.clone()), Box::new(val_typ.clone())),
            4.0,
        )]);
        self.apply_rule(node_id, &rule, rules_env);
    }

    /**
     * Apply the index-assign rule to the given variable.
     * This rule states that when we index a given variable with an integer and we assign
     * a value to this slice, the variable is a list (all other sequence types are immutable).
     */
    pub fn apply_index_assign_rule(&mut self, node_id: &str, typ: &Type, rules_env: &mut RuleEnv) {
        let rule = Vec::from([(Type::List(Box::new(typ.clone())), 3.0)]);
        self.apply_rule(node_id, &rule, rules_env);
    }

    pub fn apply_rule(&mut self, node_id: &str, rule: &Rule, rules_env: &mut RuleEnv) {
        let mut heuristics = match rules_env.get(node_id) {
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
        rules_env.insert(node_id.to_string(), heuristics);
    }
}

#[cfg(test)]
mod tests;
