use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

/**
 * Types.
 */
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

// TODO: Change free_type_var and apply to be in a trait

/**
 * Helper to convert the given type to a free type variable.
 */
pub fn free_type_var(typ: &Type) -> HashSet<String> {
    match typ {
        Type::Scheme(scheme) => {
            let typ = &scheme.type_name;
            let bounded_vars = &scheme.bounded_vars;
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
pub fn instantiate(scheme: &Scheme, mut fresh_var_generator: FreshVariableGenerator) -> Type {
    let mut instantiate_sub = Sub::new();
    for var in &scheme.bounded_vars {
        instantiate_sub.insert(
            var.to_string(),
            Type::TypeVar(TypeVar(fresh_var_generator.next())),
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
 * Helper to find the most general unifier for two given types.
 */
pub fn unify(t1: &Type, t2: &Type) {}

/**
 * Tests!
 */
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
    use super::*;

    #[test]
    fn test_instantiate_with_no_free_vars() {
        let fresh_var_generator = FreshVariableGenerator::new("v");
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

        let expected_type = Type::FuncType(FuncTypeValue {
            input: Box::new(Type::TypeVar(TypeVar("v0".to_string()))),
            output: Box::new(Type::TypeVar(TypeVar("v1".to_string()))),
        });

        let expected_res = Type::Scheme(Scheme {
            type_name: Box::new(expected_type),
            bounded_vars: bounded_vars.clone(),
        });
        let actual_res = instantiate(&scheme, fresh_var_generator);
        assert_eq!(expected_res, actual_res);
    }

    #[test]
    fn test_instantiate_with_free_vars() {
        let fresh_var_generator = FreshVariableGenerator::new("v");
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
        let actual_res = instantiate(&scheme, fresh_var_generator);
        assert_eq!(expected_res, actual_res);
    }
}
