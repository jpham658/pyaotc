use crate::type_inference::*;

//  ====================================================
//                  TYPE INFERENCE TESTS
//  ====================================================

#[cfg(test)]
mod inferrer_tests {
    use super::*;
    use malachite_bigint::BigInt;
    use rustpython_parser::{
        self,
        ast::{
            Arg, ArgWithDefault, Arguments, ExprBinOp, ExprContext, ExprName, Identifier, Operator,
            StmtAssign, Suite, TextSize,
        },
        parse,
        text_size::TextRange,
        Parse,
    };

    const DEFAULT_RANGE: TextRange = TextRange::new(TextSize::new(0), TextSize::new(1));
    const DEFAULT_NAME_CTX: ExprContext = ExprContext::Load;

    #[test]
    fn test_infer_literal() {
        let mut env = TypeEnv::new();
        let lit = ExprConstant {
            range: DEFAULT_RANGE,
            kind: None,
            value: Constant::Bool(true),
        };
        let mut type_inferrer = TypeInferrer::new();
        let expr = Expr::Constant(lit);
        let expected_res = (Sub::new(), Type::ConcreteType(ConcreteValue::Bool));
        assert_eq!(
            Ok(expected_res),
            type_inferrer.infer_expression(&mut env, &expr)
        );
    }

    #[test]
    fn test_infer_name() {
        let mut env = TypeEnv::new();
        let mut type_inferrer = TypeInferrer::new();
        env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                bounded_vars: BTreeSet::from(["a".to_string()]),
            },
        );
        let name = ExprName {
            range: DEFAULT_RANGE,
            ctx: DEFAULT_NAME_CTX,
            id: Identifier::new("x"),
        };
        let expr = Expr::Name(name);
        let res = type_inferrer.infer_expression(&mut env, &expr);

        match res {
            Ok(typ) => {
                assert_eq!(Sub::new(), typ.0);
                if let Type::Scheme(scheme) = typ.1 {
                    if let Type::TypeVar(..) = &*scheme.type_name {
                        // do nothing, pass test
                    } else {
                        panic!("Instantiation should replace bound variables with free type variables.")
                    }
                } else {
                    panic!("Instantiation should generate a scheme.")
                }
            }
            Err(e) => panic!("{:?}", e),
        }
    }

    #[test]
    fn test_infer_call() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        env.insert(
            "add".to_string(),
            Scheme {
                type_name: Box::new(Type::FuncType(FuncTypeValue {
                    input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                    output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                })),
                bounded_vars: BTreeSet::new(),
            },
        );
        let call = ExprCall {
            range: DEFAULT_RANGE,
            func: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("add"),
                ctx: DEFAULT_NAME_CTX,
            })),
            args: vec![Expr::Constant(ExprConstant {
                value: Constant::Int(BigInt::new(malachite_bigint::Sign::Plus, [5].to_vec())),
                range: DEFAULT_RANGE,
                kind: None,
            })],
            keywords: vec![],
        };

        let (subs, inferred_type) = type_inferrer
            .infer_call(&mut env, &call)
            .expect("Call should be valid.");

        assert!(subs.len() != 0);
        assert_eq!(Type::ConcreteType(ConcreteValue::Int), inferred_type);
    }

    #[test]
    fn test_infer_call_with_more_than_one_argument() {
        let mut type_inferencer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let add_funcdef = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::FuncType(FuncTypeValue {
                input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            })),
        });
        env.insert(
            "add".to_string(),
            Scheme {
                type_name: Box::new(add_funcdef),
                bounded_vars: BTreeSet::new(),
            },
        );

        // call add(2, 3)
        let call = ExprCall {
            range: DEFAULT_RANGE,
            func: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("add"),
                ctx: DEFAULT_NAME_CTX,
            })),
            args: vec![
                Expr::Constant(ExprConstant {
                    value: generate_ast_int(5),
                    range: DEFAULT_RANGE,
                    kind: None,
                }),
                Expr::Constant(ExprConstant {
                    value: generate_ast_int(3),
                    range: DEFAULT_RANGE,
                    kind: None,
                }),
            ],
            keywords: vec![],
        };

        let (subs, inferred_type) = type_inferencer
            .infer_call(&mut env, &call)
            .expect("Call should be valid.");

        assert!(subs.len() != 0);
        assert_eq!(inferred_type, Type::ConcreteType(ConcreteValue::Int));
    }

    #[test]
    fn test_infer_call_type_error() {
        let mut type_inferencer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let add_funcdef = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::FuncType(FuncTypeValue {
                input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            })),
        });
        env.insert(
            "add".to_string(),
            Scheme {
                type_name: Box::new(add_funcdef),
                bounded_vars: BTreeSet::new(),
            },
        );

        // call add(5, "hello")
        let call = ExprCall {
            range: DEFAULT_RANGE,
            func: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("add"),
                ctx: DEFAULT_NAME_CTX,
            })),
            args: vec![
                Expr::Constant(ExprConstant {
                    value: generate_ast_int(5),
                    range: DEFAULT_RANGE,
                    kind: None,
                }),
                Expr::Constant(ExprConstant {
                    range: DEFAULT_RANGE,
                    value: Constant::Str("hello".to_string()),
                    kind: None,
                }),
            ],
            keywords: vec![],
        };

        let result = type_inferencer.infer_call(&mut env, &call);

        assert!(result.is_err());
    }

    #[test]
    fn test_infer_function() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let args = generate_func_args(&["x"]);
        let return_stmt = Expr::BinOp(ExprBinOp {
            range: DEFAULT_RANGE,
            left: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("x"),
                ctx: DEFAULT_NAME_CTX,
            })),
            op: Operator::Add,
            right: Box::new(Expr::Constant(ExprConstant {
                range: DEFAULT_RANGE,
                value: generate_ast_int(1),
                kind: None,
            })),
        });
        let body: Vec<Stmt> = vec![Stmt::Return(StmtReturn {
            range: DEFAULT_RANGE,
            value: Some(Box::new(return_stmt.clone())),
        })];

        // def increment(x):
        //    return x + 1
        let func_def = StmtFunctionDef {
            range: DEFAULT_RANGE,
            name: Identifier::new("increment"),
            args: Box::new(args),
            body: body.clone(),
            decorator_list: vec![],
            returns: None,
            type_comment: None,
            type_params: vec![],
        };

        let (subs, inferred_type) = type_inferrer
            .infer_function(&mut env, &func_def)
            .expect("Funcdef shouldn't cause an error.");

        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
        });

        assert!(subs.len() != 0);
        assert_eq!(expected_type, inferred_type);
    }

    #[test]
    fn test_infer_function_with_no_args() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let body = vec![Stmt::Return(StmtReturn {
            range: DEFAULT_RANGE,
            value: None,
        })];

        // def foo():
        //    return
        let func_def = StmtFunctionDef {
            range: DEFAULT_RANGE,
            name: Identifier::new("foo"),
            args: Box::new(generate_func_args(&[])),
            body: body,
            decorator_list: vec![],
            returns: None,
            type_comment: None,
            type_params: vec![],
        };

        let (sub, inferred_type) = type_inferrer
            .infer_function(&mut env, &func_def)
            .expect("Funcdef should not cause an error.");

        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::None)),
            output: Box::new(Type::ConcreteType(ConcreteValue::None)),
        });

        assert!(sub.len() == 0);
        assert_eq!(expected_type, inferred_type);
    }

    #[test]
    fn test_infer_function_with_multiple_args() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let args = generate_func_args(&["x", "y"]);
        let binop_right = Expr::BinOp(ExprBinOp {
            range: DEFAULT_RANGE,
            left: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("y"),
                ctx: DEFAULT_NAME_CTX,
            })),
            op: Operator::Add,
            right: Box::new(Expr::Constant(ExprConstant {
                range: DEFAULT_RANGE,
                value: generate_ast_int(1),
                kind: None,
            })),
        });
        let return_stmt = Expr::BinOp(ExprBinOp {
            range: DEFAULT_RANGE,
            left: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("x"),
                ctx: DEFAULT_NAME_CTX,
            })),
            op: Operator::Add,
            right: Box::new(binop_right),
        });
        let body = vec![Stmt::Return(StmtReturn {
            range: DEFAULT_RANGE,
            value: Some(Box::new(return_stmt.clone())),
        })];

        // TODO: for some generic function def add(x,y): return x + y,
        // how can we represent that x and y can multiple types?
        // e.g int -> int, str -> int (but not int -> str), float -> int
        // def add(x, y):
        //    return x + y + 1
        let func_def = StmtFunctionDef {
            range: DEFAULT_RANGE,
            name: Identifier::new("add"),
            args: Box::new(args),
            body: body,
            decorator_list: vec![],
            returns: None,
            type_comment: None,
            type_params: vec![],
        };

        let (sub, inferred_type) = type_inferrer
            .infer_function(&mut env, &func_def)
            .expect("Funcdef shouldn't cause an error.");
        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::FuncType(FuncTypeValue {
                input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            })),
        });

        assert!(sub.len() != 0);
        assert_eq!(expected_type, inferred_type);
    }

    #[test]
    fn test_infer_function_with_no_return() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let args = generate_func_args(&["x"]);
        let binop = Expr::BinOp(ExprBinOp {
            range: DEFAULT_RANGE,
            left: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("x"),
                ctx: DEFAULT_NAME_CTX,
            })),
            op: Operator::Add,
            right: Box::new(Expr::Constant(ExprConstant {
                range: DEFAULT_RANGE,
                value: generate_ast_int(1),
                kind: None,
            })),
        });
        let body: Vec<Stmt> = vec![Stmt::Expr(StmtExpr {
            range: DEFAULT_RANGE,
            value: Box::new(binop),
        })];

        // def foo(x):
        //    x + 1
        let func_def = StmtFunctionDef {
            range: DEFAULT_RANGE,
            name: Identifier::new("foo"),
            args: Box::new(args),
            body: body,
            decorator_list: vec![],
            returns: None,
            type_comment: None,
            type_params: vec![],
        };

        let (sub, inferred_type) = type_inferrer
            .infer_function(&mut env, &func_def)
            .expect("Funcdef shouldn't cause an error.");
        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::ConcreteType(ConcreteValue::None)),
        });

        assert!(sub.len() != 0);
        assert_eq!(expected_type, inferred_type);
    }

    #[test]
    fn test_infer_function_with_different_return_types() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let factorial_fn = r#"
def return_string_or_bool(x):
    if x == 0:
        return False
    else:
        return "True"
        "#;
        let ast = Suite::parse(&factorial_fn, "<embedded>").unwrap();
        let (_, inferred_type) = type_inferrer
            .infer_stmt(&mut env, &ast[0])
            .expect("Funcdef shouldn't cause an error.");
        
        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::Any),
        });
        
        assert_eq!(expected_type, inferred_type);
    }

    #[test]
    fn test_infer_assignment() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let assign = Stmt::Assign(StmtAssign {
            range: DEFAULT_RANGE,
            targets: vec![Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("x"),
                ctx: ExprContext::Store,
            })],
            value: Box::new(Expr::Constant(ExprConstant {
                range: DEFAULT_RANGE,
                value: Constant::Float(3.0),
                kind: None,
            })),
            type_comment: None,
        });
        let (_, inferred_type) = type_inferrer
            .infer_stmt(&mut env, &assign)
            .expect("Assignment should not cause an error.");
        let expected_type = Type::Scheme(Scheme {
            type_name: Box::new(Type::ConcreteType(ConcreteValue::Float)),
            bounded_vars: BTreeSet::new(),
        });
        assert_eq!(expected_type, inferred_type);
    }

    #[test]
    fn infer_range_call() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        let x_id = (TextSize::new(3), TextSize::new(4));
        env.insert(
            x_id,
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("v0".to_string()))),
                bounded_vars: BTreeSet::new(),
            },
        );
        let call = ExprCall {
            range: DEFAULT_RANGE,
            func: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("range"),
                ctx: DEFAULT_NAME_CTX,
            })),
            args: vec![Expr::Name(ExprName {
                ctx: DEFAULT_NAME_CTX,
                id: Identifier::new("x"),
                range: DEFAULT_RANGE,
            })],
            keywords: vec![],
        };
        let assign = Stmt::Assign(StmtAssign {
            range: DEFAULT_RANGE,
            targets: vec![Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("y"),
                ctx: ExprContext::Store,
            })],
            value: Box::new(Expr::Call(call)),
            type_comment: None,
        });
        let (sub, inferred_type) = type_inferrer
            .infer_stmt(&mut env, &assign)
            .expect("Inferrence should not fail.");
        let expected_type = Type::Scheme(Scheme {
            type_name: Box::new(Type::Sequence(Box::new(Type::ConcreteType(
                ConcreteValue::Int,
            )))),
            bounded_vars: BTreeSet::new(),
        });
        let expected_sub = Sub::from([("v0".to_string(), Type::ConcreteType(ConcreteValue::Int))]);
        assert_eq!(expected_sub, sub);
        assert_eq!(expected_type, inferred_type);
    }

    /**
     * Inferrence test helpers.
     */

    fn generate_func_args(args: &[&str]) -> Arguments {
        let args = args
            .into_iter()
            .map(|id| ArgWithDefault {
                range: DEFAULT_RANGE.into(),
                def: Arg {
                    range: DEFAULT_RANGE,
                    arg: Identifier::new(*id),
                    annotation: None,
                    type_comment: None,
                },
                default: None,
            })
            .collect::<Vec<_>>();

        Arguments {
            range: DEFAULT_RANGE.into(),
            args: args,
            posonlyargs: vec![],
            vararg: None,
            kwonlyargs: vec![],
            kwarg: None,
        }
    }

    fn generate_ast_int(num: u32) -> Constant {
        Constant::Int(BigInt::new(malachite_bigint::Sign::Plus, [num].to_vec()))
    }
}

#[cfg(test)]
mod free_type_var_tests {
    use super::*;

    #[test]
    fn test_free_type_var_with_typevars() {
        let typevar = Type::TypeVar(TypeVar("n".to_string()));
        let mut expected_ftv = BTreeSet::new();
        expected_ftv.insert("n".to_string());
        assert_eq!(expected_ftv, free_type_var(&typevar));
    }

    #[test]
    fn test_free_type_var_with_functypes() {
        let input_type = Type::TypeVar(TypeVar("a".to_string()));
        let output_type = Type::TypeVar(TypeVar("b".to_string()));
        let functype = Type::FuncType(FuncTypeValue {
            input: Box::new(input_type),
            output: Box::new(output_type),
        });
        let mut expected_ftv = BTreeSet::new();
        expected_ftv.insert("a".to_string());
        expected_ftv.insert("b".to_string());
        assert_eq!(expected_ftv, free_type_var(&functype));
    }

    #[test]
    fn test_free_type_var_with_concrete_type() {
        let concrete_type = Type::ConcreteType(ConcreteValue::Float);
        let expected_ftv = BTreeSet::new();
        assert_eq!(expected_ftv, free_type_var(&concrete_type));
    }

    #[test]
    fn test_free_type_var_with_scheme() {
        // create a scheme with function type a -> b where a is bound
        // and b is free
        let input_type = Type::TypeVar(TypeVar("a".to_string()));
        let output_type = Type::TypeVar(TypeVar("b".to_string()));
        let functype = Type::FuncType(FuncTypeValue {
            input: Box::new(input_type),
            output: Box::new(output_type),
        });
        let mut bounded_vars = BTreeSet::new();
        bounded_vars.insert("b".to_string());
        let scheme = Type::Scheme(Scheme {
            type_name: Box::new(functype),
            bounded_vars: bounded_vars,
        });
        let mut expected_ftv = BTreeSet::new();
        expected_ftv.insert("a".to_string());
        assert_eq!(expected_ftv, free_type_var(&scheme));
    }
}

#[cfg(test)]
mod apply_tests {
    use super::*;

    #[test]
    fn test_apply_with_scheme() {
        let mut sub = Sub::new();
        sub.insert("a".to_string(), Type::TypeVar(TypeVar("c".to_string())));

        let type_name = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });
        let mut bounded_vars = BTreeSet::new();
        bounded_vars.insert("b".to_string());
        let typ = Type::Scheme(Scheme {
            type_name: Box::new(type_name),
            bounded_vars: bounded_vars,
        });

        let expected_subbed_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("c".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });

        let mut expected_bounded_vars = BTreeSet::new();
        expected_bounded_vars.insert("b".to_string());

        let expected_subbed_scheme = Type::Scheme(Scheme {
            type_name: Box::new(expected_subbed_type),
            bounded_vars: expected_bounded_vars,
        });

        assert_eq!(expected_subbed_scheme, apply(&sub, &typ));
    }

    #[test]
    fn test_apply_with_typevar() {
        let mut sub = Sub::new();
        sub.insert("a".to_string(), Type::ConcreteType(ConcreteValue::None));

        let typ = Type::TypeVar(TypeVar("a".to_string()));

        let expected_subbed_type = Type::ConcreteType(ConcreteValue::None);

        assert_eq!(expected_subbed_type, apply(&sub, &typ));
    }

    #[test]
    fn test_apply_with_func_type() {
        let mut sub = Sub::new();
        sub.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Int));
        sub.insert("b".to_string(), Type::ConcreteType(ConcreteValue::Bool));

        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });

        let expected_subbed_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::ConcreteType(ConcreteValue::Bool)),
        });

        assert_eq!(expected_subbed_type, apply(&sub, &typ));
    }
}

#[cfg(test)]
mod compose_subs_tests {
    use super::*;

    #[test]
    fn test_compose_subs() {
        let mut s1 = Sub::new();
        s1.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Int));
        s1.insert("b".to_string(), Type::ConcreteType(ConcreteValue::Bool));
        let mut s2 = Sub::new();
        s2.insert("x".to_string(), Type::TypeVar(TypeVar("a".to_string())));
        s2.insert("y".to_string(), Type::TypeVar(TypeVar("b".to_string())));
        let mut expected_comp_sub = Sub::new();
        expected_comp_sub.insert("x".to_string(), Type::ConcreteType(ConcreteValue::Int));
        expected_comp_sub.insert("y".to_string(), Type::ConcreteType(ConcreteValue::Bool));
        expected_comp_sub.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Int));
        expected_comp_sub.insert("b".to_string(), Type::ConcreteType(ConcreteValue::Bool));
        assert_eq!(expected_comp_sub, compose_subs(&s1, &s2));
    }

    #[test]
    fn test_compose_subs_with_two_empty_sub_maps() {
        let s1 = Sub::new();
        let s2 = Sub::new();
        let expected_comp_sub = Sub::new();
        assert_eq!(expected_comp_sub, compose_subs(&s1, &s2));
    }

    #[test]
    fn test_compose_subs_with_empty_first_sub() {
        let s1 = Sub::new();
        let mut s2 = Sub::new();
        s2.insert("x".to_string(), Type::TypeVar(TypeVar("a".to_string())));
        s2.insert("y".to_string(), Type::TypeVar(TypeVar("b".to_string())));
        let mut expected_comp_sub = Sub::new();
        expected_comp_sub.insert("x".to_string(), Type::TypeVar(TypeVar("a".to_string())));
        expected_comp_sub.insert("y".to_string(), Type::TypeVar(TypeVar("b".to_string())));
        assert_eq!(expected_comp_sub, compose_subs(&s1, &s2));
    }

    #[test]
    fn test_compose_subs_with_empty_second_sub() {
        let mut s1 = Sub::new();
        s1.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Int));
        s1.insert("b".to_string(), Type::ConcreteType(ConcreteValue::Bool));
        let s2 = Sub::new();
        let mut expected_comp_sub = Sub::new();
        expected_comp_sub.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Int));
        expected_comp_sub.insert("b".to_string(), Type::ConcreteType(ConcreteValue::Bool));
        assert_eq!(expected_comp_sub, compose_subs(&s1, &s2));
    }
}

#[cfg(test)]
mod type_env_tests {
    use super::*;

    #[test]
    fn test_free_type_vars_in_type_env_with_no_free_variables() {
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Float)),
                bounded_vars: BTreeSet::new(),
            },
        );
        assert_eq!(BTreeSet::new(), free_type_vars_in_type_env(&type_env));
    }

    #[test]
    fn test_free_type_vars_in_type_env() {
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                bounded_vars: BTreeSet::new(),
            },
        );
        let mut expected_ftvs = BTreeSet::new();
        expected_ftvs.insert("a".to_string());
        assert_eq!(expected_ftvs, free_type_vars_in_type_env(&type_env));
    }

    #[test]
    fn test_apply_to_type_env() {
        let mut sub = Sub::new();
        sub.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Str));
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                bounded_vars: BTreeSet::new(),
            },
        );
        let mut expected_type_env = TypeEnv::new();
        expected_type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Str)),
                bounded_vars: BTreeSet::new(),
            },
        );
        apply_to_type_env(&sub, &type_env);
    }
}

#[cfg(test)]
mod generalise_tests {
    use super::*;

    /**
     * Testcase for no free variables in the type and type environment.
     */
    #[test]
    fn test_generalise_with_no_free_vars() {
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                bounded_vars: BTreeSet::new(),
            },
        );
        type_env.insert(
            "y".to_string(),
            Scheme {
                type_name: Box::new(Type::FuncType(FuncTypeValue {
                    input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                    output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                })),
                bounded_vars: BTreeSet::new(),
            },
        );

        let typ = Type::ConcreteType(ConcreteValue::Int);
        let expected_scheme = Scheme {
            type_name: Box::new(typ.clone()),
            bounded_vars: BTreeSet::new(),
        };
        let actual_scheme = generalise(&typ, &type_env);
        assert_eq!(expected_scheme, actual_scheme);
    }

    /**
     * Testcase for a type with one unbound variable a and
     * a type environment where a is free.
     */
    #[test]
    fn test_generalise_with_free_vars() {
        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
                bounded_vars: BTreeSet::new(),
            },
        );

        let mut type_vars = BTreeSet::new();
        type_vars.insert("a".to_string());

        let expected_scheme = Scheme {
            type_name: Box::new(typ.clone()),
            bounded_vars: type_vars,
        };
        let actual_scheme = generalise(&typ, &type_env);
        assert_eq!(expected_scheme, actual_scheme);
    }

    /**
     * Testcase for a type a -> b, and a type environment where
     * variable x is bound to typevar a.
     */
    #[test]
    fn test_generalise_with_overlapping_free_vars() {
        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                bounded_vars: BTreeSet::new(),
            },
        );

        let mut expected_bounded_vars = BTreeSet::new();
        expected_bounded_vars.insert("b".to_string());
        let expected_scheme = Scheme {
            type_name: Box::new(typ.clone()),
            bounded_vars: expected_bounded_vars,
        };
        let actual_scheme = generalise(&typ, &type_env);
        assert_eq!(expected_scheme, actual_scheme);
    }

    /**
     * Complex testcase with a curried function a -> b -> int
     * and type environment where x is bound to a, and y is bound to bool type.
     */
    #[test]
    fn test_generalise_with_complex_case() {
        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::FuncType(FuncTypeValue {
                input: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
                output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            })),
        });
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                bounded_vars: BTreeSet::new(),
            },
        );
        type_env.insert(
            "y".to_string(),
            Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Bool)),
                bounded_vars: BTreeSet::new(),
            },
        );

        let mut expected_bounded_vars = BTreeSet::new();
        expected_bounded_vars.insert("b".to_string());
        let expected_scheme = Scheme {
            type_name: Box::new(typ.clone()),
            bounded_vars: expected_bounded_vars,
        };
        let actual_scheme = generalise(&typ, &type_env);
        assert_eq!(expected_scheme, actual_scheme);
    }
}

#[cfg(test)]
mod fresh_var_generator_tests {
    use super::*;

    #[test]
    fn test_fresh_var_generator() {
        let prefix = "v";
        let mut fresh_var_generator = FreshVariableGenerator::new(prefix);
        assert_eq!(0, fresh_var_generator.counter);
        let v0 = fresh_var_generator.next();
        assert_eq!("v0", v0);
        assert_eq!(1, fresh_var_generator.counter);
        let v1 = fresh_var_generator.next();
        assert_eq!("v1", v1);
        assert_eq!(2, fresh_var_generator.counter);
    }
}

#[cfg(test)]
mod instantiate_tests {

    use super::*;

    #[test]
    fn test_instantiate_with_no_free_vars() {
        let mut type_inferrer = TypeInferrer::new();
        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });
        let mut bounded_vars = BTreeSet::new();
        bounded_vars.insert("a".to_string());
        bounded_vars.insert("b".to_string());

        let scheme = Scheme {
            type_name: Box::new(typ.clone()),
            bounded_vars: bounded_vars.clone(),
        };

        let actual_res = instantiate(&scheme, &mut type_inferrer);
        match actual_res {
            Type::Scheme(scheme) => {
                if let Type::FuncType(FuncTypeValue {
                    input: new_a,
                    output: new_b,
                }) = *scheme.type_name
                {
                    assert!(new_a != new_b);
                } else {
                    panic!("Expected functype to be returned!")
                }
            }
            _ => panic!("Expected scheme."),
        }
    }

    #[test]
    fn test_instantiate_with_free_vars() {
        let mut type_inferrer = TypeInferrer::new();
        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });
        let mut bounded_vars = BTreeSet::new();
        bounded_vars.insert("b".to_string());

        let scheme = Scheme {
            type_name: Box::new(typ.clone()),
            bounded_vars: bounded_vars.clone(),
        };

        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("v0".to_string()))),
        });

        let expected_res = Type::Scheme(Scheme {
            type_name: Box::new(expected_type),
            bounded_vars: bounded_vars.clone(),
        });
        let actual_res = instantiate(&scheme, &mut type_inferrer);
        assert_eq!(expected_res, actual_res);
    }
}

#[cfg(test)]
mod bind_type_var_tests {
    use super::*;

    #[test]
    fn test_bind_type_var() {
        let var = "a";
        let typ = Type::ConcreteType(ConcreteValue::Int);
        let mut expected_sub = Sub::new();
        expected_sub.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Int));
        assert_eq!(Ok(expected_sub), bind_type_var(&var, &typ));
    }

    #[test]
    fn test_bind_type_var_when_typevar_is_ftv_of_given_type() {
        let var = "a";
        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::ConcreteType(ConcreteValue::Float)),
        });
        let err_msg = format!("{} occurs in {:?}.", var, typ);
        assert_eq!(
            Err(InferenceError { message: err_msg }),
            bind_type_var(&var, &typ)
        );
    }

    #[test]
    fn test_bind_type_var_when_type_is_same_typevar() {
        let var = "a";
        let typ = Type::TypeVar(TypeVar("a".to_string()));
        assert_eq!(Ok(Sub::new()), bind_type_var(var, &typ));
    }
}

#[cfg(test)]
mod unify_tests {
    use super::*;

    #[test]
    fn test_unify_with_mismatching_types() {
        let t1 = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Bool)),
            output: Box::new(Type::ConcreteType(ConcreteValue::Str)),
        });
        let t2 = Type::ConcreteType(ConcreteValue::Str);
        let err_msg = format!("Types {:?} and {:?} do not unify.", t1, t2);
        assert_eq!(Err(InferenceError { message: err_msg }), unify(&t1, &t2));
    }

    #[test]
    fn test_unify_with_type_var() {
        let t1 = Type::TypeVar(TypeVar("a".to_string()));
        let t2 = Type::ConcreteType(ConcreteValue::Bool);
        let mut expected_sub = Sub::new();
        expected_sub.insert("a".to_string(), t2.clone());
        assert_eq!(Ok(expected_sub), unify(&t1, &t2));
    }

    #[test]
    fn test_unify_with_same_concrete_types() {
        let t1 = Type::ConcreteType(ConcreteValue::Float);
        let t2 = Type::ConcreteType(ConcreteValue::Float);
        assert_eq!(Ok(Sub::new()), unify(&t1, &t2));
    }

    #[test]
    fn test_unify_with_mismatching_but_unifiable_types() {
        let t1 = Type::ConcreteType(ConcreteValue::Float);
        let t2 = Type::ConcreteType(ConcreteValue::Bool);
        assert_eq!(Ok(Sub::new()), unify(&t1, &t2));
    }

    #[test]
    fn test_unify_with_functypes() {
        let t1 = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Bool)),
            output: Box::new(Type::FuncType(FuncTypeValue {
                input: Box::new(Type::ConcreteType(ConcreteValue::Bool)),
                output: Box::new(Type::ConcreteType(ConcreteValue::Str)),
            })),
        });
        let t2 = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::FuncType(FuncTypeValue {
                input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
            })),
        });
        let mut expected_sub = Sub::new();
        expected_sub.insert("a".to_string(), Type::ConcreteType(ConcreteValue::Bool));
        expected_sub.insert("b".to_string(), Type::ConcreteType(ConcreteValue::Str));
        assert_eq!(Ok(expected_sub), unify(&t1, &t2));
    }
}
