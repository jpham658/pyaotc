use std::{collections::HashMap, fmt::{Debug, Formatter}};

use rustpython_parser::ast::{Expr, ExprCall, ExprName, Identifier, Stmt, StmtExpr};

#[derive(Debug)]
pub struct TypeRuleError {
    message:  &'static str,
}

type TypeRuleResult = Result<HashMap<String, Vec<Heuristic>>, TypeRuleError>;

pub fn build_variable_rules(ast: &[Stmt]) -> TypeRuleResult {
    let mut variable_type_map: HashMap<String, Vec<Heuristic>> = HashMap::new();
    for statement in ast {
        match statement  {
            Stmt::Expr( StmtExpr{ value, .. }) => {
                match &**value {
                    Expr::Call( ExprCall{ func, args, .. }) => {
                        let funcname = &func.as_name_expr().expect("Error finding name of function.").id;
                        if args.len() == 1 {
                            // Implement 1 arg rules here
                            let var = args.get(0)
                                                 .expect("Could not get first arg.");
                            match var {
                                Expr::Name(ExprName{ id, .. }) => {
                                    let heuristics = len_rule(funcname);
                                    let old_heuristics = variable_type_map.entry(id.to_string()).or_insert_with(Vec::new);
                                    old_heuristics.extend(heuristics);
                                }
                                _ => return Err(TypeRuleError{ message: "Not implemented yet..." })
                            }
                        }
                    },
                    _ => return Err(TypeRuleError{ message: "Not implemented yet..." })
                }
            },
            _ => {}
        }
    }
    Ok(variable_type_map)
}

/**
 * Rule: For a variable x, if len(x) is called, then x is either a set or a sequence
 */
pub fn len_rule(funcname: &Identifier) -> Vec<Heuristic> {
    let mut heuristics: Vec<Heuristic> = Vec::new();
    if funcname.as_str() == "len" {
        // TODO: Figure out better way to do this...
        heuristics.push(Heuristic{
            typename: Type::Sequence,
            score: 100.0
        });
        heuristics.push(Heuristic{
            typename: Type::Set,
            score: 100.0
        });
    }
    return heuristics;
}

#[derive(Debug)]
pub enum Type {
    Sequence,
    Set,
    Constant
}

enum ConstantType {
    None,
    Bool,
    Str,
    Bytes,
    Int,
    Tuple,
    Float,
    Complex,
    Ellipsis,
}

enum SequenceType {
    String,
    List,
    Tuple,
    Range
}

enum SetType {
    Set,
}

pub struct Heuristic {
    typename: Type,
    score: f64,
}

impl Debug for Heuristic {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Heuristic")
            .field("typename", &self.typename)
            .field("score", &self.score)
            .finish()
    }
}

// impl Debug for ConstantType {
//     fn fmt(&self, f: &mut Formatter) -> Result {
//         match self {
//             ConstantType::None => write!(f, "None"),
//             ConstantType::Bool => write!(f, "Bool"),
//             ConstantType::Str => write!(f, "Str"),
//             ConstantType::Float => write!(f, "Float"),
//             ConstantType::Int => write!(f, "Int"),
//             ConstantType::Tuple => write!(f, "Tuple"),
//             ConstantType::Ellipsis => write!(f, "Ellipsis"),
//             ConstantType::Bytes => write!(f, "Bytes"),
//             ConstantType::Complex => write!(f, "Complex"),
//         }
//     }
// }

