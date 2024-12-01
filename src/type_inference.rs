use std::{
    any::Any,
    collections::{HashMap, HashSet},
    hash::Hash,
};

use rustpython_parser::ast::{
    Constant, Expr, ExprCall, ExprConstant, Stmt, StmtExpr, StmtFunctionDef, StmtReturn,
};

//  ====================================================
//                      ERROR TYPES
//  ====================================================

//TODO: Refactor errors
#[derive(Debug, PartialEq)]
pub struct InferenceError {
    message: String,
}

//  ====================================================
//                          TYPES
//  ====================================================

#[derive(Debug, PartialEq, Clone)]
pub enum ConcreteValue {
    // TODO: Extend for other constant types
    Int,
    Bool,
    Str,
    Float,
    None,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub struct TypeVar(String);

#[derive(PartialEq, Debug, Clone)]
pub struct FuncTypeValue {
    pub input: Box<Type>,
    pub output: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConcreteType(ConcreteValue);

#[derive(Debug, Clone)]
pub struct FuncType(FuncTypeValue);

#[derive(Debug, PartialEq, Clone)]
pub struct Scheme {
    pub type_name: Box<Type>,
    pub bounded_vars: HashSet<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Any,
    ConcreteType(ConcreteValue),
    TypeVar(TypeVar),
    FuncType(FuncTypeValue),
    Scheme(Scheme),
    Undefined,
}

/**
 * Substitution of type vars to other types.
 */
pub type Sub = HashMap<String, Type>;

//  ====================================================
//                     TYPE INFERRER
//  ====================================================

type TypeInferenceRes = Result<(Sub, Type), InferenceError>;

pub struct TypeInferrer {
    fresh_var_generator: FreshVariableGenerator,
}

impl TypeInferrer {
    pub fn new() -> Self {
        Self {
            fresh_var_generator: FreshVariableGenerator::new("v"),
        }
    }

    /**
     * Helper to infer types of literals.
     */
    pub fn infer_literal(&mut self, lit: &ExprConstant) -> TypeInferenceRes {
        let ExprConstant {
            value: constant, ..
        } = lit;
        match constant {
            Constant::Bool(..) => Ok((Sub::new(), Type::ConcreteType(ConcreteValue::Bool))),
            Constant::Str(..) => Ok((Sub::new(), Type::ConcreteType(ConcreteValue::Str))),
            Constant::Int(..) => Ok((Sub::new(), Type::ConcreteType(ConcreteValue::Int))),
            Constant::Float(..) => Ok((Sub::new(), Type::ConcreteType(ConcreteValue::Float))),
            _ => Err(InferenceError {
                message: format!("Inference not implemented for literal {:?}", lit),
            }),
        }
    }

    /**
     * Helper to infer the type of a function call node.
     */
    pub fn infer_call(&mut self, env: &TypeEnv, call: &ExprCall) -> TypeInferenceRes {
        let func_name = &*call.func;
        let (sub1, type1) = self.infer_expression(env, func_name)?;

        // func_name is always gonna have a scheme returned
        let scheme_type1;

        match type1 {
            Type::Scheme(scheme) => scheme_type1 = scheme,
            _ => {
                return Err(InferenceError {
                    message: "Funcname should have scheme type.".to_string(),
                })
            }
        }

        let mut composite_subs = sub1.clone();
        let mut arg_types = Vec::new();

        for arg in &call.args {
            let new_env = apply_to_type_env(&composite_subs, env);
            let (sub_arg, arg_type) = self.infer_expression(&new_env, arg)?;
            composite_subs = compose_subs(&composite_subs, &sub_arg);
            arg_types.push(arg_type);
        }

        let return_type = Type::TypeVar(TypeVar(self.fresh_var_generator.next()));

        // Generate function type from the return type to the first arg type
        // This is to generate a curried function type definition...
        let expected_func_type =
            arg_types
                .into_iter()
                .rev()
                .fold(return_type.clone(), |acc, arg_type| {
                    Type::FuncType(FuncTypeValue {
                        input: Box::new(arg_type),
                        output: Box::new(acc),
                    })
                });

        let applied_type1 = &apply(&composite_subs, &scheme_type1.type_name);
        let resultant_sub = unify(applied_type1, &expected_func_type)?;

        let final_subs = compose_subs(&resultant_sub, &composite_subs);
        let final_return_type = apply(&final_subs, &return_type);
        Ok((final_subs, final_return_type))
    }

    /**
     * Helper to infer the types of a given function definition.
     */
    pub fn infer_function(&mut self, env: &TypeEnv, func: &StmtFunctionDef) -> TypeInferenceRes {
        let args = &func.args.args;
        let arg_types = args
            .iter()
            .map(|_| Type::TypeVar(TypeVar(self.fresh_var_generator.next())))
            .collect::<Vec<_>>();

        let mut extended_env = env.clone();
        for (arg, arg_type) in args.iter().zip(arg_types.iter()) {
            let arg_id = &arg.as_arg().arg;
            extended_env.insert(
                arg_id.as_str().to_string(),
                Scheme {
                    type_name: Box::new(arg_type.clone()),
                    bounded_vars: HashSet::new(),
                },
            );
        }

        let mut subs = Sub::new();

        // inferring function body
        for stmt in &func.body {
            let (sub, _) = self.infer_stmt(&extended_env, stmt)?;
            subs = compose_subs(&subs, &sub);
        }

        let return_stmts = &func
            .body
            .iter()
            .filter(|stmt| stmt.is_return_stmt())
            .collect::<Vec<_>>();

        let return_type: (Sub, Type);

        if return_stmts.len() > 0 {
            // assume all return stmts return same type for now
            if let Some(StmtReturn {
                value: return_stmt, ..
            }) = return_stmts[0].as_return_stmt()
            {
                match return_stmt {
                    None => return_type = (Sub::new(), Type::ConcreteType(ConcreteValue::None)),
                    Some(expr) => match self.infer_expression(&extended_env, expr) {
                        Ok(expr_type) => return_type = expr_type,
                        Err(e) => return Err(e),
                    },
                }
            } else {
                return Err(InferenceError {
                    message: "Return statements with non-expression statement not implemented yet."
                        .to_string(),
                });
            }
        } else {
            return_type = (Sub::new(), Type::ConcreteType(ConcreteValue::None));
        }

        // TODO: refactor
        let func_type = arg_types
            .into_iter()
            .rev()
            .fold(return_type.clone(), |acc, arg_type| {
                // Compose the substitution and create the function type
                let result_subs = compose_subs(&subs, &acc.0);
                let func_type = Type::FuncType(FuncTypeValue {
                    input: Box::new(arg_type),
                    output: Box::new(acc.1),
                });
                (result_subs, func_type)
            });

        let resultant_sub = func_type.0.clone();
        Ok((func_type.0, apply(&resultant_sub, &func_type.1)))
    }

    /**
     * Infer type of a given expression in a type environment.
     */
    pub fn infer_expression(&mut self, env: &TypeEnv, expr: &Expr) -> TypeInferenceRes {
        match expr {
            Expr::BinOp(binop) => {
                let (sub_left, left_type) = self.infer_expression(env, &binop.left)?;
                let new_env = apply_to_type_env(&sub_left, env);
                let (sub_right, right_type) = self.infer_expression(&new_env, &binop.right)?;
                let mut composite_subs = compose_subs(&sub_left, &sub_right);
                let resultant_type = match (left_type, right_type) {
                    (Type::Scheme(Scheme { type_name, .. }), typ)
                    | (typ, Type::Scheme(Scheme { type_name, .. })) => {
                        // Unify scheme type with given type
                        let unifier = unify(&type_name, &typ)?;
                        composite_subs = compose_subs(&composite_subs, &unifier);
                        typ
                    }
                    (
                        Type::ConcreteType(ConcreteValue::Int),
                        Type::ConcreteType(ConcreteValue::Int),
                    ) => Type::ConcreteType(ConcreteValue::Int),
                    (Type::ConcreteType(ConcreteValue::Float), _)
                    | (_, Type::ConcreteType(ConcreteValue::Float)) => {
                        Type::ConcreteType(ConcreteValue::Float)
                    }
                    (Type::ConcreteType(ConcreteValue::Str), _) => {
                        Type::ConcreteType(ConcreteValue::Str)
                    }
                    (typ, Type::ConcreteType(ConcreteValue::Str)) => {
                        if let Type::ConcreteType(val) = typ {
                            match val {
                                ConcreteValue::Str => Type::ConcreteType(ConcreteValue::Str),
                                _ => {
                                    return Err(InferenceError {
                                        message: "Cannot perform operation with string on RHS."
                                            .to_string(),
                                    })
                                }
                            }
                        } else {
                            Type::ConcreteType(ConcreteValue::Str)
                        }
                    }
                    _ => {
                        return Err(InferenceError {
                            message: "Expression type not implemented yet.".to_string(),
                        })
                    }
                };

                Ok((composite_subs, resultant_type))
            }
            Expr::Constant(lit) => self.infer_literal(lit),
            Expr::Name(name) => {
                let var = name.id.as_str();
                match env.get(var) {
                    Some(scheme) => {
                        let typ = instantiate(scheme, self);
                        return Ok((Sub::new(), typ));
                    }
                    None => Err(InferenceError {
                        message: format!("Variable {} is unbound.", var),
                    }),
                }
            }
            Expr::Call(call) => self.infer_call(env, call),
            _ => Err(InferenceError {
                message: format!(
                    "Inferrence not implemented for expression {:?}.",
                    expr.type_id()
                ),
            }),
        }
    }

    pub fn infer_stmt(&mut self, env: &TypeEnv, stmt: &Stmt) -> TypeInferenceRes {
        match stmt {
            // TODO: Implement let rule for assignments and funcdefs
            // Stmt::Assign(..) => {},
            Stmt::FunctionDef(funcdef) => self.infer_function(env, funcdef),
            Stmt::Expr(StmtExpr { value, .. }) => self.infer_expression(env, &**value),
            Stmt::Return(StmtReturn { value, .. }) => match value {
                Some(expr) => self.infer_expression(env, expr),
                _ => Err(InferenceError {
                    message: format!(
                        "Inferrence not implemented for functype {:?}.",
                        value.type_id()
                    ),
                }),
            },
            _ => Err(InferenceError {
                message: format!(
                    "Inferrence not implemented for statement {:?}.",
                    stmt.type_id()
                ),
            }),
        }
    }
}

//  ====================================================
//                  HELPER FUNCTIONS
//  ====================================================
// TODO: Change free_type_var and apply to be in a trait

/**
 * Helper to convert the given type to a free type variable.
 */
pub fn free_type_var(typ: &Type) -> HashSet<String> {
    match typ {
        Type::Scheme(scheme) => {
            let typ = &scheme.type_name;
            free_type_var(&typ)
                .difference(&scheme.bounded_vars)
                .cloned()
                .collect()
        }
        Type::TypeVar(var) => {
            let mut set = HashSet::new();
            set.insert(var.0.to_string());
            set
        }
        Type::FuncType(func_type) => {
            let input_ftv = free_type_var(&*func_type.input);
            let output_ftv = free_type_var(&*func_type.output);
            input_ftv.union(&output_ftv).cloned().collect()
        }
        _ => HashSet::new(),
    }
}

/**
 * Helper function to apply substitutions to a given type.
 */
pub fn apply(sub: &Sub, typ: &Type) -> Type {
    let type_clone = typ.clone();
    match typ {
        Type::Scheme(scheme) => {
            let bounded_vars = &scheme.bounded_vars;
            // remove quantified typevars from being subbed
            let filtered_sub: Sub = (sub.clone())
                .into_iter()
                .filter(|(key, _)| !bounded_vars.contains(key))
                .collect::<HashMap<_, _>>();
            Type::Scheme(Scheme {
                type_name: Box::new(apply(&filtered_sub, &scheme.type_name)),
                bounded_vars: bounded_vars.clone(),
            })
        }
        Type::TypeVar(typevar) => match sub.get(&typevar.0) {
            Some(t) => t.clone(),
            None => type_clone,
        },
        Type::FuncType(FuncTypeValue { input, output }) => {
            let subbed_input = apply(sub, &input);
            let subbed_output = apply(sub, &output);
            Type::FuncType(FuncTypeValue {
                input: Box::new(subbed_input),
                output: Box::new(subbed_output),
            })
        }
        _ => type_clone,
    }
}

/**
 * Helper to compose two substitutions s1 and s2.
 */
pub fn compose_subs(s1: &Sub, s2: &Sub) -> Sub {
    let applied_s2: Sub = (s2.clone())
        .into_iter()
        .map(|(var, typ)| (var, apply(s1, &typ)))
        .collect::<HashMap<_, _>>();
    (s1.clone()).into_iter().chain(applied_s2).collect()
}

pub type TypeEnv = HashMap<String, Scheme>;

/**
 * Helper to remove binding for a given variable var
 * from a type environment.
 */
pub fn remove(var: &str, env: &TypeEnv) -> TypeEnv {
    let mut new_env = env.clone();
    new_env.remove(var);
    new_env
}

/**
 * Helper to get all free type variables for a type environment.
 */
pub fn free_type_vars_in_type_env(env: &TypeEnv) -> HashSet<String> {
    env.values()
        .into_iter()
        .map(|scheme| free_type_var(&Type::Scheme(scheme.clone())))
        .flatten()
        .collect::<HashSet<_>>()
}

/**
 * Helper to apply a substitution for a given type environment.
 */
pub fn apply_to_type_env(sub: &Sub, env: &TypeEnv) -> TypeEnv {
    env.into_iter()
        .filter_map(|(key, val)| {
            let subbed_scheme = apply(sub, &Type::Scheme(val.clone()));
            if let Type::Scheme(scheme) = subbed_scheme {
                Some((key.clone(), scheme))
            } else {
                None
            }
        })
        .collect::<HashMap<String, Scheme>>()
}

/**
 * Helper to generalise a type given a type environment.
 */
pub fn generalise(typ: &Type, env: &TypeEnv) -> Scheme {
    let free_vars_in_env: HashSet<String> = free_type_var(typ)
        .difference(&free_type_vars_in_type_env(env))
        .cloned()
        .collect();
    Scheme {
        type_name: Box::new(typ.clone()),
        bounded_vars: free_vars_in_env,
    }
}

/**
 * Helper to generate fresh variable names
 */
pub struct FreshVariableGenerator {
    pub prefix: String,
    pub counter: usize,
}

impl FreshVariableGenerator {
    pub fn new(prefix: &str) -> Self {
        FreshVariableGenerator {
            prefix: prefix.to_string(),
            counter: 0,
        }
    }

    pub fn next(&mut self) -> String {
        let fresh_var = format!("{}{}", self.prefix, self.counter);
        self.counter += 1;
        fresh_var
    }
}

/**
 * Helper to instantiate a given scheme.
 */
pub fn instantiate(scheme: &Scheme, type_inferrer: &mut TypeInferrer) -> Type {
    let mut instantiate_sub = Sub::new();
    for var in &scheme.bounded_vars {
        instantiate_sub.insert(
            var.to_string(),
            Type::TypeVar(TypeVar(type_inferrer.fresh_var_generator.next())),
        );
    }
    // substitute here cos apply only substitutes FTVs
    let new_type_name = apply(&instantiate_sub, &scheme.type_name);
    Type::Scheme(Scheme {
        type_name: Box::new(new_type_name),
        bounded_vars: scheme.bounded_vars.clone(),
    })
}

/**
 * Helper to bind a type variable to a type, returning a substitution.
 * Checks if a variable is being bound to itself and also if the type
 * variable we are trying to bind is a free type variable of the given type.
 */
pub fn bind_type_var(var: &str, typ: &Type) -> Result<Sub, InferenceError> {
    match typ {
        Type::TypeVar(typevar) => {
            if typevar.0 == var {
                return Ok(Sub::new());
            }
        }
        _ => {}
    }

    if free_type_var(typ).contains(var) {
        let err_msg = format!("{} occurs in {:?}.", var, typ);
        return Err(InferenceError { message: err_msg });
    }

    let mut sub = Sub::new();
    sub.insert(var.to_string(), typ.clone());

    Ok(sub)
}

/**
 * Helper to find the most general unifier for two given types.
 */
pub fn unify(t1: &Type, t2: &Type) -> Result<Sub, InferenceError> {
    match (t1, t2) {
        (Type::FuncType(t1_func_value), Type::FuncType(t2_func_value)) => {
            let FuncTypeValue {
                input: t1_input,
                output: t1_output,
            } = t1_func_value;
            let FuncTypeValue {
                input: t2_input,
                output: t2_output,
            } = t2_func_value;

            let sub1_res = unify(t1_input, t2_input);
            let sub2_res;

            match sub1_res {
                Ok(sub1) => {
                    sub2_res = unify(&apply(&sub1, t1_output), &apply(&sub1, t2_output));

                    match sub2_res {
                        Ok(sub2) => Ok(compose_subs(&sub1, &sub2)),
                        Err(e) => Err(e),
                    }
                }
                Err(e) => Err(e),
            }
        }
        (Type::TypeVar(TypeVar(var)), typ) | (typ, Type::TypeVar(TypeVar(var))) => {
            bind_type_var(var, typ)
        }
        (Type::ConcreteType(val1), Type::ConcreteType(val2)) => {
            if val1 != val2 {
                return Err(InferenceError {
                    message: "Mismatching concrete types".to_string(),
                });
            }
            Ok(Sub::new())
        }
        _ => {
            let err_msg = format!("Types {:?} and {:?} do not unify.", t1, t2);
            Err(InferenceError { message: err_msg })
        }
    }
}

//  ====================================================
//                        TESTS
//  ====================================================

#[cfg(test)]
mod inferrer_tests {
    use std::env::args;

    use super::*;
    use malachite_bigint::BigInt;
    use rustpython_parser::{
        ast::{
            Arg, ArgWithDefault, Arguments, ExprBinOp, ExprContext, ExprName, Identifier, Operator,
            TextSize,
        },
        text_size::TextRange,
    };

    const DEFAULT_RANGE: TextRange = TextRange::new(TextSize::new(0), TextSize::new(1));
    const DEFAULT_NAME_CTX: ExprContext = ExprContext::Load;

    #[test]
    fn test_literal_inferrence() {
        let env = TypeEnv::new();
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
            type_inferrer.infer_expression(&env, &expr)
        );
    }

    #[test]
    fn test_name_inferrence() {
        let mut env = TypeEnv::new();
        let mut type_inferrer = TypeInferrer::new();
        env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                bounded_vars: HashSet::from(["a".to_string()]),
            },
        );
        let name = ExprName {
            range: DEFAULT_RANGE,
            ctx: DEFAULT_NAME_CTX,
            id: Identifier::new("x"),
        };
        let expr = Expr::Name(name);
        let res = type_inferrer.infer_expression(&env, &expr);

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
    fn test_call_inferrence() {
        let mut type_inferrer = TypeInferrer::new();
        let mut env = TypeEnv::new();
        env.insert(
            "add".to_string(),
            Scheme {
                type_name: Box::new(Type::FuncType(FuncTypeValue {
                    input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                    output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                })),
                bounded_vars: HashSet::new(),
            },
        );
        let call = ExprCall {
            range: DEFAULT_RANGE,
            func: Box::new(Expr::Name(ExprName {
                range: DEFAULT_RANGE,
                id: Identifier::new("add"),
                ctx: ExprContext::Load,
            })),
            args: vec![Expr::Constant(ExprConstant {
                value: Constant::Int(BigInt::new(malachite_bigint::Sign::Plus, [5].to_vec())),
                range: DEFAULT_RANGE,
                kind: None,
            })],
            keywords: vec![],
        };

        let (subs, inferred_type) = type_inferrer
            .infer_call(&env, &call)
            .expect("Call should be valid.");

        assert!(subs.len() != 0);
        assert_eq!(Type::ConcreteType(ConcreteValue::Int), inferred_type);
    }

    #[test]
    fn test_call_inferrence_with_more_than_one_argument() {
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
                bounded_vars: HashSet::new(),
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
            .infer_call(&env, &call)
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
                bounded_vars: HashSet::new(),
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

        let result = type_inferencer.infer_call(&env, &call);

        assert!(result.is_err());
    }

    #[test]
    fn test_infer_function() {
        let mut type_inferrer = TypeInferrer::new();
        let env = TypeEnv::new();
        let args = Arguments {
            range: DEFAULT_RANGE.into(),
            args: vec![ArgWithDefault {
                range: DEFAULT_RANGE.into(),
                def: Arg {
                    range: DEFAULT_RANGE,
                    arg: Identifier::new("x"),
                    annotation: None,
                    type_comment: None,
                },
                default: None,
            }],
            posonlyargs: vec![],
            vararg: None,
            kwonlyargs: vec![],
            kwarg: None,
        };
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
            value: Some(Box::new(return_stmt)),
        })];

        let func_def = StmtFunctionDef {
            range: DEFAULT_RANGE,
            name: Identifier::new("increment"),
            args: Box::new(args),
            body: body,
            decorator_list: vec![],
            returns: None,
            type_comment: None,
            type_params: vec![],
        };

        let (subs, inferred_type) = type_inferrer
            .infer_function(&env, &func_def)
            .expect("Funcdef shouldn't cause an error.");

        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::ConcreteType(ConcreteValue::Int)),
            output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
        });

        assert!(subs.len() != 0);
        assert_eq!(expected_type, inferred_type);
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
        let mut expected_ftv = HashSet::new();
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
        let mut expected_ftv = HashSet::new();
        expected_ftv.insert("a".to_string());
        expected_ftv.insert("b".to_string());
        assert_eq!(expected_ftv, free_type_var(&functype));
    }

    #[test]
    fn test_free_type_var_with_concrete_type() {
        let concrete_type = Type::ConcreteType(ConcreteValue::Float);
        let expected_ftv = HashSet::new();
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
        let mut bounded_vars = HashSet::new();
        bounded_vars.insert("b".to_string());
        let scheme = Type::Scheme(Scheme {
            type_name: Box::new(functype),
            bounded_vars: bounded_vars,
        });
        let mut expected_ftv = HashSet::new();
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
        let mut bounded_vars = HashSet::new();
        bounded_vars.insert("b".to_string());
        let typ = Type::Scheme(Scheme {
            type_name: Box::new(type_name),
            bounded_vars: bounded_vars,
        });

        let expected_subbed_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("c".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });

        let mut expected_bounded_vars = HashSet::new();
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
                bounded_vars: HashSet::new(),
            },
        );
        assert_eq!(HashSet::new(), free_type_vars_in_type_env(&type_env));
    }

    #[test]
    fn test_free_type_vars_in_type_env() {
        let mut type_env = TypeEnv::new();
        type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                bounded_vars: HashSet::new(),
            },
        );
        let mut expected_ftvs = HashSet::new();
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
                bounded_vars: HashSet::new(),
            },
        );
        let mut expected_type_env = TypeEnv::new();
        expected_type_env.insert(
            "x".to_string(),
            Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Str)),
                bounded_vars: HashSet::new(),
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
                bounded_vars: HashSet::new(),
            },
        );
        type_env.insert(
            "y".to_string(),
            Scheme {
                type_name: Box::new(Type::FuncType(FuncTypeValue {
                    input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
                    output: Box::new(Type::ConcreteType(ConcreteValue::Int)),
                })),
                bounded_vars: HashSet::new(),
            },
        );

        let typ = Type::ConcreteType(ConcreteValue::Int);
        let expected_scheme = Scheme {
            type_name: Box::new(typ.clone()),
            bounded_vars: HashSet::new(),
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
                bounded_vars: HashSet::new(),
            },
        );

        let mut type_vars = HashSet::new();
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
                bounded_vars: HashSet::new(),
            },
        );

        let mut expected_bounded_vars = HashSet::new();
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
                bounded_vars: HashSet::new(),
            },
        );
        type_env.insert(
            "y".to_string(),
            Scheme {
                type_name: Box::new(Type::ConcreteType(ConcreteValue::Bool)),
                bounded_vars: HashSet::new(),
            },
        );

        let mut expected_bounded_vars = HashSet::new();
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
    use std::any::Any;

    use super::*;

    #[test]
    fn test_instantiate_with_no_free_vars() {
        let mut type_inferrer = TypeInferrer::new();
        let typ = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("a".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("b".to_string()))),
        });
        let mut bounded_vars = HashSet::new();
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
        let mut bounded_vars = HashSet::new();
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
    fn test_unify_with_mismatching_concrete_types() {
        let t1 = Type::ConcreteType(ConcreteValue::Float);
        let t2 = Type::ConcreteType(ConcreteValue::Bool);
        let err_msg = "Mismatching concrete types".to_string();
        assert_eq!(Err(InferenceError { message: err_msg }), unify(&t1, &t2));
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
