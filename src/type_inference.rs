use std::{
    any::Any,
    collections::{BTreeSet, HashMap},
    hash::Hash,
};

use rustpython_parser::{
    ast::{
        located::UnaryOp, Constant, Expr, ExprCall, ExprConstant, ExprName, Ranged, Stmt, StmtExpr,
        StmtFor, StmtFunctionDef, StmtIf, StmtReturn, StmtWhile,
    },
    text_size::TextRange,
};

use crate::{
    astutils::is_subscriptable,
    rule_typing::{
        get_inferred_rule_types, infer_stmts_with_rules, rule_unify_types, RuleEnv, RuleTypeDB,
    },
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

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum ConcreteValue {
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
    Range,
    List(Box<Type>),
    Set(Box<Type>),
    Mapping(Box<Type>, Box<Type>),
}

/**
 * A database that maps nodes to types.
 * Used for rule typing to find types of r-values and slices.
 * TODO: Should I extend this for every node in the AST?
 */
pub type NodeTypeDB = HashMap<TextRange, Type>;

/**
 * Substitution of type vars to other types.
 */
pub type Sub = HashMap<String, Type>;

//  ====================================================
//                     TYPE INFERRER
//  ====================================================
pub fn infer_ast_types(
    inferrer: &mut TypeInferrer,
    env: &mut TypeEnv,
    ast: &[Stmt],
    type_db: &mut NodeTypeDB,
) {
    for stmt in ast {
        match infer_types(inferrer, env, &stmt, type_db) {
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
                        } else {
                            type_db.insert(lhs_val.range(), typ);
                        }
                    }
                    // if not a named expr, deal with in rule typing.
                }
                Stmt::AnnAssign(annassign) => {
                    if let Some(name) = annassign.target.as_name_expr() {
                        let name = name.id.as_str();
                        env.insert(
                            name.to_string(),
                            Scheme {
                                type_name: Box::new(typ.clone()),
                                bounded_vars: BTreeSet::new(),
                            },
                        );
                    } else if let Some(subscript) = annassign.target.as_subscript_expr() {
                        type_db.insert(subscript.range(), typ.clone());
                    }
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
    type_db: &mut NodeTypeDB,
) -> Result<Type, InferenceError> {
    let (sub, inferred_type) = inferrer.infer_stmt(env, stmt, type_db)?;
    for (_, typ) in type_db.iter_mut() {
        *typ = apply(&sub, typ);
    }
    Ok(apply(&sub, &inferred_type))
}

type TypeInferenceRes = Result<(Sub, Type), InferenceError>;

pub struct TypeInferrer {
    fresh_var_generator: FreshVariableGenerator,
    ret_types: Vec<Vec<Type>>, // stack to keep track of return types in function
}

impl TypeInferrer {
    pub fn new() -> Self {
        Self {
            fresh_var_generator: FreshVariableGenerator::new("v"),
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
    pub fn infer_call(
        &mut self,
        env: &mut TypeEnv,
        call: &ExprCall,
        type_db: &mut NodeTypeDB,
    ) -> TypeInferenceRes {
        let func = &*call.func;

        // test if call is an attribute
        if func.is_attribute_expr() {
            let func_attr = func.as_attribute_expr().unwrap();
            let attr = &func_attr.value;
            if !attr.is_name_expr() {
                return Err(InferenceError {
                    message: "Invalid function call.".to_string(),
                });
            }

            // TODO: Figure out how to map different calls e.g list.pop() to their types..
            let mut sub = Sub::new();
            for arg in &call.args {
                let mut new_env = apply_to_type_env(&sub, env);
                let (arg_sub, arg_type) = self.infer_expression(&mut new_env, arg, type_db)?;
                type_db.insert(arg.range(), arg_type);
                sub = compose_subs(&arg_sub, &sub);
            }
            return Ok((sub, Type::Range));
        }

        if !func.is_name_expr() {
            return Err(InferenceError {
                message: "Invalid function call.".to_string(),
            });
        }

        let func_name = func.as_name_expr().unwrap().id.as_str();
        if func_name.eq("print") {
            return Ok((Sub::new(), Type::ConcreteType(ConcreteValue::None)));
        }

        if func_name.eq("len") {
            return Ok((Sub::new(), Type::ConcreteType(ConcreteValue::Int)));
        }

        if func_name.eq("range") {
            let int_type = Type::ConcreteType(ConcreteValue::Int);
            let mut sub = Sub::new();
            for arg in &call.args {
                let mut new_env = apply_to_type_env(&sub, env);
                let (arg_sub, arg_type) = self.infer_expression(&mut new_env, arg, type_db)?;
                let unifier = match unify(&arg_type, &int_type) {
                    Ok(sub) => sub,
                    Err(..) => Sub::new(),
                };
                sub = compose_subs(&arg_sub, &unifier);
            }
            return Ok((sub, Type::Range));
        }

        let (sub1, type1) = self.infer_expression(env, func, type_db)?;

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
            let (sub_arg, arg_type) = self.infer_expression(&mut new_env, arg, type_db)?;
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
    pub fn infer_function(
        &mut self,
        env: &mut TypeEnv,
        func: &StmtFunctionDef,
        type_db: &mut NodeTypeDB,
    ) -> TypeInferenceRes {
        let mut rule_env = RuleEnv::new();
        let mut rule_type_db = RuleTypeDB::new();

        let args = &func.args.args;
        let arg_types = args
            .iter()
            .map(|arg| match &arg.as_arg().annotation {
                None => Type::TypeVar(TypeVar(self.fresh_var_generator.next())),
                Some(expr) => verify_type_ann(&expr).unwrap().1, // get type
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
            let (sub, _) = self.infer_stmt(&mut extended_env, stmt, type_db)?;
            subs = compose_subs(&subs, &sub);
            extended_env = apply_to_type_env(&subs, &mut extended_env);
        }
        apply_to_type_db(&subs, type_db);

        // infer with rules too
        if let Err(e) =
            infer_stmts_with_rules(&mut rule_env, &func.body, &mut rule_type_db, type_db)
        {
            eprintln!("RuleInferrenceError: {}", e.message);
            return Err(InferenceError { message: e.message });
        }

        let inferred_rule_types = get_inferred_rule_types(&mut rule_env);

        // unify all types inferred from rules with types in the type env
        for (name, scheme) in &inferred_rule_types {
            let curr_type = extended_env.get(name).unwrap();
            let unifier = unify(&scheme.type_name, &curr_type.type_name)?;
            subs = compose_subs(&subs, &unifier);
        }

        println!("inferred rule types {:?}", inferred_rule_types);

        // update arg types with rule-inferred types if necessary
        let mut updated_arg_types = arg_types.clone();
        for (arg, original_type) in args.iter().zip(arg_types.iter()) {
            let arg_name = arg.as_arg().arg.as_str().to_string();

            if let Some(rule_scheme) = inferred_rule_types.get(&arg_name) {
                let rule_type = &rule_scheme.type_name;
                let unified_type = rule_unify_types(original_type, rule_type);
                let arg_index = args
                    .iter()
                    .position(|a| a.as_arg().arg == arg_name)
                    .unwrap();
                updated_arg_types[arg_index] = unified_type.clone();

                println!(
                    "Updated argument `{}` type: {:?} -> {:?}",
                    arg_name, original_type, unified_type
                );
            } else {
                let arg_index = args
                    .iter()
                    .position(|a| a.as_arg().arg == arg_name)
                    .unwrap();
                updated_arg_types[arg_index] = original_type.clone();
                println!(
                    "Keeping original argument `{}` type: {:?}",
                    arg_name, original_type
                );
            }
        }

        let return_type: Type;

        if let Some(typexpr) = &func.returns {
            return_type = verify_type_ann(&typexpr)?.1;
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

        let func_type = if updated_arg_types.len() == 0 {
            let return_type = match return_type {
                Type::Scheme(Scheme { type_name, .. }) => *type_name,
                _ => return_type,
            };

            Type::FuncType(FuncTypeValue {
                input: Box::new(Type::ConcreteType(ConcreteValue::None)),
                output: Box::new(return_type),
            })
        } else {
            updated_arg_types
                .into_iter()
                .rev()
                .fold(return_type.clone(), |typ, arg_type| {
                    let typ = match typ {
                        Type::Scheme(Scheme { type_name, .. }) => *type_name,
                        _ => typ,
                    };

                    match arg_type {
                        Type::Scheme(Scheme { type_name, .. }) => Type::FuncType(FuncTypeValue {
                            input: type_name.clone(),
                            output: Box::new(typ),
                        }),
                        _ => Type::FuncType(FuncTypeValue {
                            input: Box::new(arg_type),
                            output: Box::new(typ),
                        }),
                    }
                })
        };

        let resultant_sub = subs.clone();
        Ok((resultant_sub, apply(&subs, &func_type)))
    }

    /**
     * Infer type of a given expression in a type environment.
     */
    pub fn infer_expression(
        &mut self,
        env: &mut TypeEnv,
        expr: &Expr,
        type_db: &mut NodeTypeDB,
    ) -> TypeInferenceRes {
        match expr {
            Expr::BinOp(binop) => {
                let (sub_left, left_type) = self.infer_expression(env, &binop.left, type_db)?;
                let mut new_env = apply_to_type_env(&sub_left, env);
                let (sub_right, right_type) =
                    self.infer_expression(&mut new_env, &binop.right, type_db)?;
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
            Expr::Call(call) => self.infer_call(env, call, type_db),
            Expr::BoolOp(bop) => {
                // TODO: consider types of bop -> although might be a bit more difficult considering
                // there aren't really predictable patterns apart from operands are truthy/falsy
                Ok((Sub::new(), Type::ConcreteType(ConcreteValue::Bool)))
            }
            Expr::Compare(cmp) => {
                let (left_sub, left_type) = self.infer_expression(env, &*cmp.left, type_db)?;
                let mut composed_sub = left_sub;
                for comparator in &cmp.comparators {
                    let (comparator_sub, comparator_type) =
                        self.infer_expression(env, comparator, type_db)?;

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
                let (sub, typ) = self.infer_expression(env, &uop.operand, type_db)?;
                let unifier = unify(&typ, &inferred_type)?;
                let composed_sub = compose_subs(&sub, &unifier);
                Ok((composed_sub, inferred_type))
            }
            Expr::Subscript(subscript) => {
                if let Some(typ) = type_db.get(&subscript.range()) {
                    return Ok((Sub::new(), typ.clone()));
                }

                if let Some(typ) = type_db.get(&subscript.value.range()) {
                    match typ {
                        Type::TypeVar(..) => {}
                        _ => return get_elt_type(typ),
                    }
                }

                if let Some(name) = subscript.value.as_name_expr() {
                    let id = name.id.as_str();
                    if let Some(scheme) = env.get(id) {
                        match *scheme.type_name {
                            Type::TypeVar(..) => {}
                            _ => return get_elt_type(&scheme.type_name),
                        }
                    }
                }

                let (slice_sub, slice_type) =
                    match self.infer_expression(env, &subscript.slice, type_db)? {
                        (sub, Type::Scheme(Scheme { type_name, .. })) => (sub, *type_name),
                        (sub, typ) => (sub, typ),
                    };

                let mut new_env = apply_to_type_env(&slice_sub, env);
                let (value_sub, value_type) =
                    match self.infer_expression(&mut new_env, &subscript.value, type_db)? {
                        (sub, Type::Scheme(Scheme { type_name, .. })) => (sub, *type_name),
                        (sub, typ) => (sub, typ),
                    };

                let subscript_type = Type::TypeVar(TypeVar(self.fresh_var_generator.next()));
                let value_mapping_type = Type::Mapping(
                    Box::new(slice_type.clone()),
                    Box::new(subscript_type.clone()),
                );

                let value_unifier = unify(&value_mapping_type, &value_type)?;

                type_db.insert(subscript.value.range(), value_mapping_type.clone());
                type_db.insert(subscript.slice.range(), slice_type.clone());
                type_db.insert(subscript.range(), subscript_type.clone());

                let resultant_sub =
                    compose_subs(&value_unifier, &compose_subs(&value_sub, &slice_sub));

                Ok((resultant_sub, subscript_type))
            }
            Expr::List(list) => {
                if list.elts.is_empty() {
                    let typevar = Type::TypeVar(TypeVar(self.fresh_var_generator.next()));
                    return Ok((Sub::new(), typevar));
                }
                let (mut elt_sub, elt_type) = self.infer_expression(env, &list.elts[0], type_db)?;
                for idx in 1..list.elts.len() {
                    let (idx_sub, idx_type) =
                        self.infer_expression(env, &list.elts[idx], type_db)?;
                    if idx_type != elt_type {
                        return Err(InferenceError {
                            message: "Cannot have list with elements of differing types."
                                .to_string(),
                        });
                    }
                    elt_sub = compose_subs(&elt_sub, &idx_sub);
                }
                Ok((elt_sub, Type::List(Box::new(elt_type))))
            }
            Expr::Set(set) => {
                if set.elts.is_empty() {
                    let typevar = Type::TypeVar(TypeVar(self.fresh_var_generator.next()));
                    return Ok((Sub::new(), typevar));
                }
                let (mut elt_sub, elt_type) = self.infer_expression(env, &set.elts[0], type_db)?;
                for idx in 1..set.elts.len() {
                    let (idx_sub, idx_type) =
                        self.infer_expression(env, &set.elts[idx], type_db)?;
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

    pub fn infer_stmt(
        &mut self,
        env: &mut TypeEnv,
        stmt: &Stmt,
        type_db: &mut NodeTypeDB,
    ) -> TypeInferenceRes {
        match stmt {
            Stmt::AnnAssign(annassign) => {
                let rhs = annassign
                    .value
                    .clone()
                    .expect("RHS of assignment is missing.");
                let (mut rhs_sub, rhs_type) = self.infer_expression(env, &rhs, type_db)?;
                let (_, var_type) = verify_type_ann(&annassign.annotation)?;
                match unify(&rhs_type, &var_type) {
                    Ok(sub) => {
                        rhs_sub = compose_subs(&rhs_sub, &sub);
                    }
                    Err(..) => {
                        return Err(InferenceError {
                            message: "RHS of assignment does not match type annotation."
                                .to_string(),
                        });
                    }
                }
                if let Some(id) = annassign.target.as_name_expr() {
                    env.insert(id.id.as_str().to_string(), generalise(&var_type, &env));
                } else {
                    type_db.insert(annassign.target.range(), var_type.clone());
                }

                Ok((rhs_sub, var_type))
            }
            Stmt::Assign(assign) => {
                let (rhs_sub, rhs_type) = self.infer_expression(env, &assign.value, type_db)?;
                let mut new_env = env.clone();
                let var;
                let target = &assign.targets[0];
                match target {
                    Expr::Name(name) => {
                        var = name.id.as_str();
                        new_env = remove(&var, env);
                    }
                    Expr::Subscript(subscript) => {
                        let (subscript_sub, subscript_type) =
                            self.infer_expression(env, target, type_db)?;

                        let subscript_unifier = unify(&subscript_type, &rhs_type)?;

                        // unify value_type's value with subscript_type
                        let value_type = type_db.get(&subscript.value.range()).unwrap();
                        let value_unifier = match value_type {
                            Type::Mapping(_, v_type) => unify(&v_type, &subscript_type)?,
                            Type::List(elt_type) => unify(&elt_type, &subscript_type)?,
                            Type::Range => {
                                unify(&subscript_type, &Type::ConcreteType(ConcreteValue::Int))?
                            }
                            Type::ConcreteType(ConcreteValue::Str) => {
                                unify(&subscript_type, &Type::ConcreteType(ConcreteValue::Str))?
                            }
                            _ => {
                                let msg =
                                    format!("Value type {:?} is not subscriptable.", value_type);
                                return Err(InferenceError { message: msg });
                            }
                        };

                        let resultant_sub = compose_subs(
                            &subscript_sub,
                            &compose_subs(&subscript_unifier, &value_unifier),
                        );

                        return Ok((resultant_sub, rhs_type));
                    }
                    _ => {
                        let err_msg = format!(
                            "Left hand side of assignment must be a variable or subscript."
                        );
                        return Err(InferenceError { message: err_msg });
                    }
                }
                let general_type = generalise(&rhs_type, &mut new_env);
                env.insert(var.to_string(), general_type.clone());

                Ok((rhs_sub, rhs_type))
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
                let func_type = self.infer_function(&mut new_env, funcdef, type_db);
                match &func_type {
                    Ok((_, typ)) => {
                        let generalised_functype = generalise(typ, env);
                        env.insert(funcdef.name.as_str().to_string(), generalised_functype);
                        func_type
                    }
                    Err(_) => func_type,
                }
            }
            Stmt::For(StmtFor {
                iter,
                body,
                orelse,
                target,
                ..
            }) => {
                // TODO: Am I only doing for loops with names?
                if !target.is_name_expr() {
                    return Err(InferenceError {
                        message: "Target in for loop must be a name.".to_string(),
                    });
                }

                let (iter_sub, iter_type) = self.infer_expression(env, iter, type_db)?;
                let target_name = target.as_name_expr().unwrap().id.as_str().to_string();
                let mut new_env = env.clone();
                new_env.insert(
                    target_name,
                    Scheme {
                        type_name: Box::new(iter_type.clone()),
                        bounded_vars: BTreeSet::new(),
                    },
                );

                let mut inferred_body_and_or_else = body
                    .into_iter()
                    .map(|stmt| self.infer_stmt(&mut new_env, stmt, type_db))
                    .collect::<Vec<_>>();
                let inferred_or_else = orelse
                    .into_iter()
                    .map(|stmt| self.infer_stmt(&mut new_env, stmt, type_db))
                    .collect::<Vec<_>>();
                inferred_body_and_or_else.extend(inferred_or_else);
                let composed_sub = inferred_body_and_or_else
                    .into_iter()
                    .map(|res| match res {
                        Ok((sub, _)) => sub,
                        Err(..) => Sub::new(),
                    })
                    .fold(iter_sub, |acc, sub| compose_subs(&acc, &sub));
                Ok((composed_sub, Type::ConcreteType(ConcreteValue::None)))
            }
            Stmt::While(StmtWhile {
                test, body, orelse, ..
            })
            | Stmt::If(StmtIf {
                test, body, orelse, ..
            }) => {
                let (inferred_test_sub, _) = self.infer_expression(env, test, type_db)?;
                let mut new_env = apply_to_type_env(&inferred_test_sub, env);
                let mut inferred_body_and_or_else = body
                    .into_iter()
                    .map(|stmt| self.infer_stmt(&mut new_env, stmt, type_db))
                    .collect::<Vec<_>>();
                let inferred_or_else = orelse
                    .into_iter()
                    .map(|stmt| self.infer_stmt(&mut new_env, stmt, type_db))
                    .collect::<Vec<_>>();
                inferred_body_and_or_else.extend(inferred_or_else);
                let composed_sub = inferred_body_and_or_else
                    .into_iter()
                    .map(|res| match res {
                        Ok((sub, _)) => sub,
                        Err(..) => Sub::new(),
                    })
                    .fold(inferred_test_sub, |acc, sub| compose_subs(&acc, &sub));
                Ok((composed_sub, Type::ConcreteType(ConcreteValue::None)))
            }
            Stmt::Expr(StmtExpr { value, .. }) => self.infer_expression(env, &**value, type_db),
            Stmt::Return(StmtReturn { value, .. }) => match value {
                Some(expr) => {
                    let inferred_type = self.infer_expression(env, expr, type_db);
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
 * Helper to resolve element type if the given type is
 * indexable, otherwise return an error.
 */
pub fn get_elt_type(typ: &Type) -> TypeInferenceRes {
    match typ {
        Type::ConcreteType(ConcreteValue::Str) => return Ok((Sub::new(), typ.clone())),
        Type::Set(elt_typ) | Type::List(elt_typ) => return Ok((Sub::new(), *elt_typ.clone())),
        Type::Mapping(_, val_typ) => return Ok((Sub::new(), *val_typ.clone())),
        _ => {
            let msg = format!("Type {:?} is not subscriptable.", typ);
            return Err(InferenceError { message: msg });
        }
    }
}

/**
 * Helper to apply subs to all types in type DB
 */
pub fn apply_to_type_db(sub: &Sub, type_db: &mut NodeTypeDB) {
    type_db.iter_mut().for_each(|(_, val)| {
        *val = apply(sub, val);
    });
}

/**
 * Helper to verify a string type annotation
 */
pub fn verify_type_ann(typexpr: &Expr) -> TypeInferenceRes {
    let typ = match &typexpr {
        Expr::Constant(ExprConstant { value, .. }) => match value {
            Constant::Str(str) => parse_str_type_ann(str),
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

pub fn parse_str_type_ann(annotation: &str) -> Type {
    let annotation = annotation.trim();
    if annotation == "int" {
        Type::ConcreteType(ConcreteValue::Int)
    } else if annotation == "str" {
        Type::ConcreteType(ConcreteValue::Str)
    } else if annotation == "float" {
        Type::ConcreteType(ConcreteValue::Float)
    } else if annotation == "bool" {
        Type::ConcreteType(ConcreteValue::Bool)
    } else if annotation == "None" {
        Type::ConcreteType(ConcreteValue::None)
    } else if annotation.starts_with("list[") && annotation.ends_with("]") {
        // parse list annotation
        let inner = &annotation[5..annotation.len() - 1];
        Type::List(Box::new(parse_str_type_ann(inner)))
    } else if annotation.starts_with("set[") && annotation.ends_with("]") {
        // parse set annotation
        let inner = &annotation[5..annotation.len() - 1];
        Type::Set(Box::new(parse_str_type_ann(inner)))
    } else if annotation.starts_with("dict[") && annotation.ends_with("]") {
        // parse dict annotation
        let inner = &annotation[5..annotation.len() - 1];
        let parts: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();

        if parts.len() != 2 {
            panic!("Invalid dict annotation: {}", annotation);
        }

        let key_type = parse_str_type_ann(parts[0]);
        let value_type = parse_str_type_ann(parts[1]);

        Type::Mapping(Box::new(key_type), Box::new(value_type))
    } else {
        panic!("Invalid type annotation: {}", annotation);
    }
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
        Type::Mapping(key_type, val_type) => {
            let subbed_key_type = apply(sub, &key_type);
            let subbed_val_type = apply(sub, &val_type);
            Type::Mapping(Box::new(subbed_key_type), Box::new(subbed_val_type))
        }
        Type::List(elt_type) => {
            let subbed_elt_type = apply(sub, elt_type);
            Type::List(Box::new(subbed_elt_type))
        }
        Type::Set(elt_type) => {
            let subbed_elt_type = apply(sub, elt_type);
            Type::Set(Box::new(subbed_elt_type))
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
pub fn apply_to_type_env(sub: &Sub, env: &mut TypeEnv) -> TypeEnv {
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
        (Type::Mapping(key_type, val_type), Type::List(elt_type))
        | (Type::List(elt_type), Type::Mapping(key_type, val_type)) => {
            if let Type::ConcreteType(ConcreteValue::Int) = **key_type {
                unify(&val_type, &elt_type)
            } else {
                let err_msg = format!("Types {:?} and {:?} do not unify.", t1, t2);
                Err(InferenceError { message: err_msg })
            }
        }
        (Type::Mapping(key_type, val_type), Type::Range)
        | (Type::Range, Type::Mapping(key_type, val_type)) => {
            if let Type::ConcreteType(ConcreteValue::Int) = **key_type {
                unify(&val_type, &Type::ConcreteType(ConcreteValue::Int))
            } else {
                let err_msg = format!("Types {:?} and {:?} do not unify.", t1, t2);
                Err(InferenceError { message: err_msg })
            }
        }
        (Type::Mapping(key_type, val_type), Type::ConcreteType(ConcreteValue::Str))
        | (Type::ConcreteType(ConcreteValue::Str), Type::Mapping(key_type, val_type)) => {
            if let Type::ConcreteType(ConcreteValue::Int) = **key_type {
                unify(&val_type, &Type::ConcreteType(ConcreteValue::Str))
            } else {
                let err_msg = format!("Types {:?} and {:?} do not unify.", t1, t2);
                Err(InferenceError { message: err_msg })
            }
        }
        (Type::Mapping(k1, v1), Type::Mapping(k2, v2)) => {
            let key_unifier = unify(k1, k2)?;
            let val_unifier = unify(v1, v2)?;

            Ok(compose_subs(&key_unifier, &val_unifier))
        }
        (Type::List(typ1), Type::List(typ2)) => unify(&typ1, &typ2),
        _ => {
            let err_msg = format!("Types {:?} and {:?} do not unify.", t1, t2);
            Err(InferenceError { message: err_msg })
        }
    }
}

#[cfg(test)]
mod tests;
