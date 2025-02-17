use rustpython_parser::{
    ast::{located::ExprContext, TextSize},
    text_size::TextRange,
};

use crate::rule_typing::*;

#[cfg(test)]
mod get_inferred_rule_types {
    use super::*;

    #[test]
    fn test_one_variable_in_rule_env() {
        let mut rule_env: RuleEnv = HashMap::new();
        rule_env.insert(
            "x".to_string(),
            Heuristic::from([
                (Type::ConcreteType(ConcreteValue::Str), 5.0),
                (
                    Type::Sequence(Box::new(Type::TypeVar(TypeVar("r0".to_string())))),
                    4.0,
                ),
            ]),
        );

        let inferred_types = get_inferred_rule_types(&mut rule_env);

        assert_eq!(
            inferred_types.get("x"),
            Some(&Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Str)),
                bounded_vars: BTreeSet::new(),
            })
        );
    }

    #[test]
    fn test_multiple_variables_in_rule_env() {
        let mut rule_env: RuleEnv = HashMap::new();
        rule_env.insert(
            "x".to_string(),
            Heuristic::from([
                (Type::ConcreteType(ConcreteValue::Str), 5.0),
                (
                    Type::Sequence(Box::new(Type::TypeVar(TypeVar("r0".to_string())))),
                    4.0,
                ),
            ]),
        );
        rule_env.insert(
            "y".to_string(),
            Heuristic::from([
                (
                    Type::Mapping(
                        Box::new(Type::TypeVar(TypeVar("r1".to_string()))),
                        Box::new(Type::TypeVar(TypeVar("r2".to_string()))),
                    ),
                    5.0,
                ),
                (Type::ConcreteType(ConcreteValue::Str), 3.0),
            ]),
        );

        let inferred_types = get_inferred_rule_types(&mut rule_env);

        assert_eq!(
            inferred_types.get("x"),
            Some(&Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Str)),
                bounded_vars: BTreeSet::new(),
            })
        );
        assert_eq!(
            inferred_types.get("y"),
            Some(&Scheme {
                type_name: Box::new(Type::Mapping(
                    Box::new(Type::TypeVar(TypeVar("r1".to_string()))),
                    Box::new(Type::TypeVar(TypeVar("r2".to_string()))),
                )),
                bounded_vars: BTreeSet::new(),
            })
        );
    }

    #[test]
    fn test_empty_rule_env() {
        let mut rule_env: RuleEnv = HashMap::new();
        let inferred_types = get_inferred_rule_types(&mut rule_env);
        assert!(inferred_types.is_empty());
    }
}

#[cfg(test)]
mod get_most_likely_type_tests {
    use super::*;

    #[test]
    fn test_get_most_likely_type() {
        let heuristic = Heuristic::from([
            (Type::ConcreteType(ConcreteValue::Str), 5.0),
            (
                Type::Sequence(Box::new(Type::TypeVar(TypeVar("r0".to_string())))),
                3.0,
            ),
            (
                Type::Mapping(
                    Box::new(Type::TypeVar(TypeVar("r1".to_string()))),
                    Box::new(Type::TypeVar(TypeVar("r2".to_string()))),
                ),
                3.0,
            ),
        ]);
        let actual_res = get_most_likely_type(&heuristic);
        assert_eq!(Type::ConcreteType(ConcreteValue::Str), actual_res);
    }

    #[test]
    fn test_get_most_likely_type_with_same_count() {
        let heuristic = Heuristic::from([
            (Type::ConcreteType(ConcreteValue::Str), 3.0),
            (
                Type::Sequence(Box::new(Type::TypeVar(TypeVar("r0".to_string())))),
                3.0,
            ),
            (
                Type::Mapping(
                    Box::new(Type::TypeVar(TypeVar("r1".to_string()))),
                    Box::new(Type::TypeVar(TypeVar("r2".to_string()))),
                ),
                3.0,
            ),
        ]);
        let actual_res = get_most_likely_type(&heuristic);
        assert_eq!(
            Type::Mapping(
                Box::new(Type::TypeVar(TypeVar("r1".to_string()))),
                Box::new(Type::TypeVar(TypeVar("r2".to_string()))),
            ),
            actual_res
        );
    }
}

#[cfg(test)]
mod infer_stmts_with_rules_tests {
    use malachite_bigint::BigInt;
    use rustpython_parser::ast::{Constant, ExprConstant, ExprName, ExprSubscript, Identifier};

    use super::*;

    const DEFAULT_RANGE: TextRange = TextRange::new(TextSize::new(0), TextSize::new(1));
    const DEFAULT_EXPR_CTX: ExprContext = ExprContext::Load;

    #[test]
    fn test_infer_stmts_with_index_and_assign_stmt() {
        let mut rule_env = RuleEnv::new();
        let type_db = NodeTypeDB::from([
            (
                TextRange::new(TextSize::new(0), TextSize::new(2)),
                Type::ConcreteType(ConcreteValue::Int),
            ),
            (
                TextRange::new(TextSize::new(0), TextSize::new(1)),
                Type::ConcreteType(ConcreteValue::Int),
            ),
        ]);
        let subscript = ExprSubscript {
            range: TextRange::new(TextSize::new(0), TextSize::new(2)),
            slice: Box::new(Expr::Constant(ExprConstant {
                range: TextRange::new(TextSize::new(0), TextSize::new(1)),
                value: Constant::Int(BigInt::new(malachite_bigint::Sign::Plus, [2].to_vec())),
                kind: None,
            })),
            value: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("x"),
                ctx: DEFAULT_EXPR_CTX,
            })),
            ctx: DEFAULT_EXPR_CTX,
        };
        let value = ExprConstant {
            range: DEFAULT_RANGE,
            value: Constant::Int(BigInt::new(malachite_bigint::Sign::Plus, [3].to_vec())),
            kind: None,
        };
        let assignment = StmtAssign {
            range: DEFAULT_RANGE,
            targets: vec![Expr::Subscript(subscript)],
            value: Box::new(Expr::Constant(value)),
            type_comment: None,
        };

        infer_stmts_with_rules(&mut rule_env, &[Stmt::Assign(assignment)], &type_db);
        let expected_elt_type = Type::ConcreteType(ConcreteValue::Int);
        let expected_heuristics =
            Heuristic::from([(Type::Sequence(Box::new(expected_elt_type)), 3.0)]);
        let expected_rule_env = RuleEnv::from([("x".to_string(), expected_heuristics)]);
        assert_eq!(expected_rule_env, rule_env);
    }
}

#[cfg(test)]
mod infer_expr_with_rules_tests {
    use malachite_bigint::BigInt;
    use rustpython_parser::ast::{
        Constant, ExprCall, ExprConstant, ExprName, ExprSubscript, Identifier,
    };

    use super::*;

    const DEFAULT_RANGE: TextRange = TextRange::new(TextSize::new(0), TextSize::new(1));
    const DEFAULT_EXPR_CTX: ExprContext = ExprContext::Load;

    #[test]
    fn test_infer_len_call() {
        let mut rule_env = RuleEnv::new();
        let mut rule_inferrer = RuleInferrer::new();
        let call = ExprCall {
            range: DEFAULT_RANGE,
            func: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("len"),
                ctx: DEFAULT_EXPR_CTX,
            })),
            args: vec![Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("x"),
                ctx: DEFAULT_EXPR_CTX,
            })],
            keywords: vec![],
        };
        let expr = Expr::Call(call);

        infer_expr_with_rules(&mut rule_env, &expr, &mut rule_inferrer);
        let expected_type_var = Type::TypeVar(TypeVar("r0".to_string()));
        let expected_heuristics = Heuristic::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::Sequence(Box::new(expected_type_var.clone())), 2.0),
            (Type::Set(Box::new(expected_type_var.clone())), 2.0),
        ]);
        let expected_rule_env = RuleEnv::from([("x".to_string(), expected_heuristics)]);
        assert_eq!(expected_rule_env, rule_env);
    }

    #[test]
    fn test_infer_subscript_with_integer_slice() {
        let mut rule_env = RuleEnv::new();
        let mut rule_inferrer = RuleInferrer::new();
        let subscript = ExprSubscript {
            range: DEFAULT_RANGE,
            slice: Box::new(Expr::Constant(ExprConstant {
                range: DEFAULT_RANGE,
                value: Constant::Int(BigInt::new(malachite_bigint::Sign::Plus, [2].to_vec())),
                kind: None,
            })),
            value: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("x"),
                ctx: DEFAULT_EXPR_CTX,
            })),
            ctx: DEFAULT_EXPR_CTX,
        };
        let expr = Expr::Subscript(subscript);

        infer_expr_with_rules(&mut rule_env, &expr, &mut rule_inferrer);
        let expected_type_var = Type::TypeVar(TypeVar("r0".to_string()));
        let expected_heuristics = Heuristic::from([
            (Type::ConcreteType(ConcreteValue::Str), 2.0),
            (Type::Sequence(Box::new(expected_type_var.clone())), 2.0),
        ]);
        let expected_rule_env = RuleEnv::from([("x".to_string(), expected_heuristics)]);
        assert_eq!(expected_rule_env, rule_env);
    }
}
