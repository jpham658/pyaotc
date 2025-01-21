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

pub fn infer_types(
    inferrer: &mut TypeInferrer,
    env: &mut TypeEnv,
    stmt: &Stmt,
) -> Result<Type, InferenceError> {
    let (sub, inferred_type) = inferrer.infer_stmt(env, stmt)?;
    Ok(apply(&sub, &inferred_type))
}

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
            let (sub, _) = self.infer_stmt(&mut extended_env, stmt)?;
            subs = compose_subs(&subs, &sub);
        }

        let return_stmts = &func
            .body
            .iter()
            .filter(|stmt| stmt.is_return_stmt())
            .collect::<Vec<_>>();

        let return_type: (Sub, Type);

        if !return_stmts.is_empty() {
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

        let func_type = if arg_types.len() == 0 {
                let return_type_clone = return_type.clone();
                (
                    Sub::new(),
                    Type::FuncType(FuncTypeValue {
                        input: Box::new(Type::ConcreteType(ConcreteValue::None)),
                        output: Box::new(return_type_clone.1),
                    }),
                )
            } else {
                arg_types
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
                    })
            };

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

    pub fn infer_stmt(&mut self, env: &mut TypeEnv, stmt: &Stmt) -> TypeInferenceRes {
        match stmt {
            Stmt::Assign(assign) => {
                let (rhs_sub, rhs_type) = self.infer_expression(env, &assign.value)?;
                let new_env;
                let var;
                match &assign.targets[0] {
                    Expr::Name(name) => {
                        var = name.id.as_str();
                        new_env = remove(&var, env);
                    }
                    _ => {
                        let err_msg = format!("Left hand side of assignment must be a variable.");
                        return Err(InferenceError { message: err_msg });
                    }
                }
                let general_type = generalise(&rhs_type, &new_env);
                env.insert(var.to_string(), general_type.clone());

                Ok((rhs_sub, Type::Scheme(general_type)))
            }
            Stmt::FunctionDef(funcdef) => {
                let func_type = self.infer_function(env, funcdef);
                match &func_type {
                    Ok(pair) => {
                        let generalised_functype = generalise(&pair.1, env);
                        env.insert(funcdef.name.as_str().to_string(), generalised_functype);
                        func_type
                    }
                    Err(_) => func_type,
                }
            }
            Stmt::Expr(StmtExpr { value, .. }) => self.infer_expression(env, &**value),
            Stmt::Return(StmtReturn { value, .. }) => match value {
                Some(expr) => self.infer_expression(env, expr),
                None => Ok((Sub::new(), Type::ConcreteType(ConcreteValue::None))),
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

            let sub1 = unify(t1_input, t2_input)?;
            let sub2 = unify(&apply(&sub1, t1_output), &apply(&sub1, t2_output))?;

            Ok(compose_subs(&sub1, &sub2))
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

#[cfg(test)]
mod tests;
