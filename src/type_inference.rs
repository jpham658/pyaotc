use std::{
    any::Any,
    collections::{BTreeSet, HashMap, HashSet},
    hash::Hash,
};

use rustpython_parser::ast::{
    located::UnaryOp, Constant, Expr, ExprCall, ExprConstant, ExprName, Stmt, StmtExpr,
    StmtFunctionDef, StmtIf, StmtReturn, StmtWhile,
};

use crate::{astutils::GetReturnStmts, codegen::typed_codegen};

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

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ConcreteValue {
    // TODO: Extend for other constant types
    Int,
    Bool,
    Str,
    Float,
    None,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub struct TypeVar(pub String);

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub struct FuncTypeValue {
    pub input: Box<Type>,
    pub output: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConcreteType(ConcreteValue);

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct FuncType(FuncTypeValue);

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Scheme {
    pub type_name: Box<Type>,
    pub bounded_vars: BTreeSet<String>,
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Type {
    Any,
    ConcreteType(ConcreteValue),
    TypeVar(TypeVar),
    FuncType(FuncTypeValue),
    Scheme(Scheme),
    Undefined,
    Sequence(Box<Type>),
    Set(Box<Type>),
    Mapping(Box<Type>), // TODO: Technically should be <K,V>
}

/**
 * Substitution of type vars to other types.
 */
pub type Sub = HashMap<String, Type>;

//  ====================================================
//                     TYPE INFERRER
//  ====================================================
pub fn infer_ast_types(inferrer: &mut TypeInferrer, env: &mut TypeEnv, ast: &[Stmt]) {
    for stmt in ast {
        match infer_types(inferrer, env, &stmt) {
            Ok(typ) => match &stmt {
                Stmt::FunctionDef(funcdef) => {
                    let func_name = funcdef.name.as_str().to_string();
                    env.insert(
                        func_name,
                        Scheme {
                            type_name: Box::new(typ),
                            bounded_vars: BTreeSet::new(),
                        },
                    );
                }
                Stmt::Assign(assign) => {
                    if assign.targets[0].is_name_expr() {
                        let lhs_name = assign.targets[0]
                            .as_name_expr()
                            .unwrap()
                            .id
                            .as_str()
                            .to_string();
                        env.insert(
                            lhs_name,
                            Scheme {
                                type_name: Box::new(typ),
                                bounded_vars: BTreeSet::new(),
                            },
                        );
                    } else {
                        let lhs_val = &assign.targets[0].as_subscript_expr().unwrap().value;
                        if lhs_val.is_name_expr() {
                            let lhs_name = lhs_val.as_name_expr().unwrap().id.as_str().to_string();
                            env.insert(
                                lhs_name,
                                Scheme {
                                    type_name: Box::new(typ),
                                    bounded_vars: BTreeSet::new(),
                                },
                            );
                        }
                    }
                    // if not a named expr, deal with in rule typing.
                }
                _ => {}
            },
            Err(e) => {
                eprintln!("{:?}", e);
            }
        }
    }
}

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
    typename_to_type: HashMap<String, Type>,
    ret_types: Vec<Vec<Type>>, // stack to keep track of return types in function
}

impl TypeInferrer {
    pub fn new() -> Self {
        let typename_to_type = HashMap::from([
            ("bool".to_string(), Type::ConcreteType(ConcreteValue::Bool)),
            ("int".to_string(), Type::ConcreteType(ConcreteValue::Int)),
            (
                "float".to_string(),
                Type::ConcreteType(ConcreteValue::Float),
            ),
            ("str".to_string(), Type::ConcreteType(ConcreteValue::Str)),
            ("None".to_string(), Type::ConcreteType(ConcreteValue::None)),
        ]);
        Self {
            fresh_var_generator: FreshVariableGenerator::new("v"),
            typename_to_type,
            ret_types: Vec::new(),
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
            Constant::None => Ok((Sub::new(), Type::ConcreteType(ConcreteValue::None))),
            _ => Err(InferenceError {
                message: format!("Inference not implemented for literal {:?}", lit),
            }),
        }
    }

    /**
     * Helper to infer the type of a function call node.
     */
    pub fn infer_call(&mut self, env: &TypeEnv, call: &ExprCall) -> TypeInferenceRes {
        let func = &*call.func;
        // func_name will always be a name expr
        let func_name = func.as_name_expr().unwrap().id.as_str();
        if func_name.eq("print") {
            return Ok((Sub::new(), Type::ConcreteType(ConcreteValue::None)));
        }

        let (sub1, type1) = self.infer_expression(env, func)?;

        // func_name is always gonna have a scheme returned
        let scheme_type1;

        match &type1 {
            Type::Scheme(scheme) => match &*scheme.type_name {
                Type::ConcreteType(..) => return Ok((sub1, type1.clone())),
                Type::TypeVar(..) => return Ok((sub1, *scheme.type_name.clone())),
                _ => scheme_type1 = scheme,
            },
            _ => {
                return Err(InferenceError {
                    message: "Funcname should have scheme type.".to_string(),
                })
            }
        }

        let mut composite_subs = sub1.clone();
        let mut arg_types = Vec::new();

        for arg in &call.args {
            let mut new_env = apply_to_type_env(&composite_subs, env);
            let (sub_arg, arg_type) = self.infer_expression(&mut new_env, arg)?;
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

        let applied_type1 = apply(&composite_subs, &scheme_type1.type_name);
        let resultant_sub = match (&applied_type1, &expected_func_type) {
            _ => unify(&applied_type1, &expected_func_type)?,
        };
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
            .map(|arg| match &arg.as_arg().annotation {
                None => Type::TypeVar(TypeVar(self.fresh_var_generator.next())),
                Some(expr) => verify_type_ann(&expr, &self.typename_to_type).unwrap().1, // get type
            })
            .collect::<Vec<_>>();

        let mut extended_env = env.clone();
        for (arg, arg_type) in args.iter().zip(arg_types.iter()) {
            let arg_id = &arg.as_arg().arg;
            extended_env.insert(
                arg_id.as_str().to_string(),
                Scheme {
                    type_name: Box::new(arg_type.clone()),
                    bounded_vars: BTreeSet::new(),
                },
            );
        }

        let mut subs = Sub::new();
        self.ret_types.push(Vec::new());

        // inferring function body
        for stmt in &func.body {
            let (sub, _) = self.infer_stmt(&mut extended_env, stmt)?;
            subs = compose_subs(&subs, &sub);
            extended_env = apply_to_type_env(&subs, &extended_env);
        }

        let return_type: Type;

        if let Some(typexpr) = &func.returns {
            return_type = verify_type_ann(&typexpr, &self.typename_to_type)?.1;
        } else {
            let ret_types: Vec<_> = self
                .ret_types
                .pop()
                .expect("No return types vector declared for current funcdef.")
                .into_iter()
                .map(|t| apply(&subs, &t))
                .collect();

            let mut resultant_type = if ret_types.is_empty() {
                Type::ConcreteType(ConcreteValue::None)
            } else {
                ret_types[0].clone()
            };
            for typ in ret_types {
                if typ != resultant_type {
                    // Different return types
                    resultant_type = Type::Any;
                    break;
                }
            }
            return_type = resultant_type
        }

        let func_type = if arg_types.len() == 0 {
            Type::FuncType(FuncTypeValue {
                input: Box::new(Type::ConcreteType(ConcreteValue::None)),
                output: Box::new(return_type),
            })
        } else {
            arg_types
                .into_iter()
                .rev()
                .fold(return_type.clone(), |typ, arg_type| {
                    Type::FuncType(FuncTypeValue {
                        input: Box::new(arg_type),
                        output: Box::new(typ),
                    })
                })
        };

        let resultant_sub = subs.clone();
        Ok((resultant_sub, apply(&subs, &func_type)))
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
            Expr::BoolOp(bop) => {
                // TODO: consider types of bop -> although might be a bit more difficult considering
                // there aren't really predictable patterns apart from operands are truthy/falsy
                Ok((Sub::new(), Type::ConcreteType(ConcreteValue::Bool)))
            }
            Expr::Compare(cmp) => {
                let (left_sub, left_type) = self.infer_expression(env, &*cmp.left)?;
                let mut composed_sub = left_sub;
                for comparator in &cmp.comparators {
                    let (comparator_sub, comparator_type) =
                        self.infer_expression(env, comparator)?;

                    let unified_sub = match left_type.clone() {
                        Type::Scheme(scheme) => unify(&scheme.type_name, &comparator_type)?,
                        _ => unify(&left_type, &comparator_type)?,
                    };
                    composed_sub = compose_subs(&composed_sub, &unified_sub);
                }

                // The result of a comparison is always a bool
                Ok((composed_sub, Type::ConcreteType(ConcreteValue::Bool)))
            }
            Expr::UnaryOp(uop) => {
                let inferred_type = match uop.op {
                    UnaryOp::UAdd | UnaryOp::USub | UnaryOp::Invert => {
                        Type::ConcreteType(ConcreteValue::Int)
                    }
                    UnaryOp::Not => Type::ConcreteType(ConcreteValue::Bool),
                };
                let (sub, typ) = self.infer_expression(env, &uop.operand)?;
                let unifier = unify(&typ, &inferred_type)?;
                let composed_sub = compose_subs(&sub, &unifier);
                Ok((composed_sub, inferred_type))
            }
            Expr::List(list) => {
                if list.elts.is_empty() {
                    let typevar = Type::TypeVar(TypeVar(self.fresh_var_generator.next()));
                    return Ok((Sub::new(), typevar));
                }
                let (mut elt_sub, elt_type) = self.infer_expression(env, &list.elts[0])?;
                for idx in 1..list.elts.len() {
                    let (idx_sub, idx_type) = self.infer_expression(env, &list.elts[idx])?;
                    if idx_type != elt_type {
                        return Err(InferenceError {
                            message: "Cannot have list with elements of differing types."
                                .to_string(),
                        });
                    }
                    elt_sub = compose_subs(&elt_sub, &idx_sub);
                }
                Ok((elt_sub, Type::Sequence(Box::new(elt_type))))
            }
            Expr::Set(set) => {
                if set.elts.is_empty() {
                    let typevar = Type::TypeVar(TypeVar(self.fresh_var_generator.next()));
                    return Ok((Sub::new(), typevar));
                }
                let (mut elt_sub, elt_type) = self.infer_expression(env, &set.elts[0])?;
                for idx in 1..set.elts.len() {
                    let (idx_sub, idx_type) = self.infer_expression(env, &set.elts[idx])?;
                    if idx_type != elt_type {
                        return Err(InferenceError {
                            message: "Cannot have list with elements of differing types."
                                .to_string(),
                        });
                    }
                    elt_sub = compose_subs(&elt_sub, &idx_sub);
                }
                Ok((elt_sub, Type::Set(Box::new(elt_type))))
            }
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
                let target = &assign.targets[0];
                match target {
                    Expr::Name(name) => {
                        var = name.id.as_str();
                        new_env = remove(&var, env);
                    }
                    Expr::Subscript(subscript) => {
                        let (sliced_sub, _) = self.infer_expression(env, &subscript.slice)?;
                        let mut subscript_sub = compose_subs(&sliced_sub, &rhs_sub);
                        let mut subscript_type = Type::ConcreteType(ConcreteValue::None);

                        if subscript.value.is_name_expr() {
                            var = &subscript.value.as_name_expr().unwrap().id.as_str();
                            match env.get(var) {
                                None => return Ok((subscript_sub, rhs_type)), // TODO: Figure out how to do this better...
                                Some(scheme) => {
                                    let typename = scheme.type_name.clone();

                                    match *typename {
                                        Type::Sequence(..) => {
                                            subscript_sub = unify(
                                                &typename,
                                                &Type::Sequence(Box::new(rhs_type.clone())),
                                            )?;
                                            subscript_type = *typename.clone()
                                        }
                                        Type::Mapping(..) => {
                                            subscript_sub = unify(
                                                &typename,
                                                &Type::Mapping(Box::new(rhs_type.clone())),
                                            )?;
                                            subscript_type = *typename.clone()
                                        }
                                        _ => {
                                            return Err(InferenceError {
                                                message: "Type is not indexable.".to_string(),
                                            })
                                        }
                                    }
                                }
                            }
                        }
                        return Ok((subscript_sub, subscript_type));
                    }
                    _ => {
                        let err_msg = format!(
                            "Left hand side of assignment must be a variable or subscript."
                        );
                        return Err(InferenceError { message: err_msg });
                    }
                }
                let general_type = generalise(&rhs_type, &new_env);
                env.insert(var.to_string(), general_type.clone());

                Ok((rhs_sub, Type::Scheme(general_type)))
            }
            Stmt::FunctionDef(funcdef) => {
                // Add function name to env in case it is tail recursive
                let fn_name = funcdef.name.as_str();
                let mut new_env = env.clone();
                new_env.insert(
                    fn_name.to_string(),
                    Scheme {
                        type_name: Box::new(Type::TypeVar(TypeVar(
                            self.fresh_var_generator.next(),
                        ))),
                        bounded_vars: BTreeSet::new(),
                    },
                );
                let func_type = self.infer_function(&new_env, funcdef);
                match &func_type {
                    Ok((_, typ)) => {
                        let generalised_functype = generalise(typ, env);
                        env.insert(funcdef.name.as_str().to_string(), generalised_functype);
                        func_type
                    }
                    Err(_) => func_type,
                }
            }
            Stmt::While(StmtWhile {
                test, body, orelse, ..
            })
            | Stmt::If(StmtIf {
                test, body, orelse, ..
            }) => {
                let (inferred_test_sub, _) = self.infer_expression(env, test)?;
                let mut new_env = apply_to_type_env(&inferred_test_sub, env);
                let mut inferred_body_and_or_else = body
                    .into_iter()
                    .map(|stmt| self.infer_stmt(&mut new_env, stmt).unwrap())
                    .collect::<Vec<_>>();
                let inferred_or_else = orelse
                    .into_iter()
                    .map(|stmt| self.infer_stmt(&mut new_env, stmt).unwrap())
                    .collect::<Vec<_>>();
                inferred_body_and_or_else.extend(inferred_or_else);
                let composed_sub = inferred_body_and_or_else
                    .into_iter()
                    .map(|(sub, _)| sub)
                    .fold(inferred_test_sub, |acc, sub| compose_subs(&acc, &sub));
                Ok((composed_sub, Type::ConcreteType(ConcreteValue::None)))
            }
            Stmt::Expr(StmtExpr { value, .. }) => self.infer_expression(env, &**value),
            Stmt::Return(StmtReturn { value, .. }) => match value {
                Some(expr) => {
                    let inferred_type = self.infer_expression(env, expr);
                    if let Ok((_, typ)) = &inferred_type {
                        if self.ret_types.is_empty() {
                            panic!("Cannot have a return statement outside of a function.")
                        }
                        let ret_types = self.ret_types.last_mut().unwrap();
                        ret_types.push(typ.clone());
                    }
                    inferred_type
                }
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
/**
 * Helper to verify a type annotation
 */
pub fn verify_type_ann(
    typexpr: &Expr,
    typename_to_type: &HashMap<String, Type>,
) -> TypeInferenceRes {
    let typ = match &typexpr {
        Expr::Name(ExprName { id, .. }) => {
            let typename = id.as_str();
            match typename_to_type.get(typename) {
                Some(typ) => typ.clone(),
                None => {
                    return Err(InferenceError {
                        message: "Invalid type annotation.".to_string(),
                    })
                }
            }
        }
        Expr::Constant(ExprConstant { value, .. }) => match value {
            Constant::None => typename_to_type.get("None").unwrap().clone(),
            _ => {
                return Err(InferenceError {
                    message: "Invalid type annotation.".to_string(),
                })
            }
        },
        _ => {
            return Err(InferenceError {
                message: "Invalid type annotation.".to_string(),
            })
        }
    };

    Ok((Sub::new(), typ))
}

/**
 * Helper to convert the given type to a free type variable.
 */
pub fn free_type_var(typ: &Type) -> BTreeSet<String> {
    match typ {
        Type::Scheme(scheme) => {
            let typ = &scheme.type_name;
            free_type_var(&typ)
                .difference(&scheme.bounded_vars)
                .cloned()
                .collect()
        }
        Type::TypeVar(var) => {
            let mut set = BTreeSet::new();
            set.insert(var.0.to_string());
            set
        }
        Type::FuncType(func_type) => {
            let input_ftv = free_type_var(&*func_type.input);
            let output_ftv = free_type_var(&*func_type.output);
            input_ftv.union(&output_ftv).cloned().collect()
        }
        _ => BTreeSet::new(),
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
        Type::Sequence(elt_type) => {
            let subbed_elt_type = apply(sub, elt_type);
            let seq_type = Type::Sequence(Box::new(subbed_elt_type));
            Type::Scheme(Scheme {
                type_name: Box::new(seq_type),
                bounded_vars: BTreeSet::new(),
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
pub fn free_type_vars_in_type_env(env: &TypeEnv) -> BTreeSet<String> {
    env.values()
        .into_iter()
        .map(|scheme| free_type_var(&Type::Scheme(scheme.clone())))
        .flatten()
        .collect::<BTreeSet<_>>()
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
    let free_vars_in_env: BTreeSet<String> = free_type_var(typ)
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
        (Type::Scheme(Scheme { type_name, .. }), typ)
        | (typ, Type::Scheme(Scheme { type_name, .. })) => unify(type_name, typ),
        (Type::TypeVar(TypeVar(var)), typ) | (typ, Type::TypeVar(TypeVar(var))) => {
            bind_type_var(var, typ)
        }
        (Type::ConcreteType(val1), Type::ConcreteType(val2)) => {
            if val1 != val2
                && (val1.eq(&ConcreteValue::None)
                    || val2.eq(&ConcreteValue::None)
                    || val1.eq(&ConcreteValue::Str)
                    || val2.eq(&ConcreteValue::Str))
            {
                return Err(InferenceError {
                    message: "Mismatching concrete types".to_string(),
                });
            }
            Ok(Sub::new())
        }
        (Type::Sequence(typ1), Type::Sequence(typ2)) => unify(&typ1, &typ2),
        _ => {
            let err_msg = format!("Types {:?} and {:?} do not unify.", t1, t2);
            Err(InferenceError { message: err_msg })
        }
    }
}

#[cfg(test)]
mod tests;
