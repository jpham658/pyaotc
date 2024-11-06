use std::{collections::HashMap, fmt::{Debug, Formatter, Result}};

use rustpython_parser::ast::{Constant, ExprBinOp, Stmt, StmtExpr};

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

pub struct Heuristic {
    typename: ConstantType,
    score: f64,
}

impl Debug for Heuristic {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Heuristic")
         .field("typename", &self.typename)
         .field("score", &self.score)
         .finish()
    }
}

impl Debug for ConstantType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ConstantType::None => write!(f, "None"),
            ConstantType::Bool => write!(f, "Bool"),
            ConstantType::Str => write!(f, "Str"),
            ConstantType::Float => write!(f, "Float"),
            ConstantType::Int => write!(f, "Int"),
            ConstantType::Tuple => write!(f, "Tuple"),
            ConstantType::Ellipsis => write!(f, "Ellipsis"),
            ConstantType::Bytes => write!(f, "Bytes"),
            ConstantType::Complex => write!(f, "Complex"),
        }
    }
}

pub fn apply_rules(ast: &[Stmt]) -> HashMap<&str, Vec<Heuristic>> {
    // TODO: refactor
    // TODO: research how to generate heuristics map that is shared
    // Apply all rules that we have defined on the
    // AST and return a hashmap of all variables 
    // and their most likely type
    let mut vars: HashMap<&str, Vec<Heuristic>> = HashMap::new();
    for statement in ast {
        // Should look like this at end
        // {
        //      "x": [ { Float, INF } ]
        // }
        match statement {
            Stmt::Expr( StmtExpr{ value, .. } ) => {
                if value.is_bin_op_expr() {
                    let binop= value.as_bin_op_expr().unwrap();
                    let l_heuristic;
                    let r_heuristic;

                    if binop.left.is_name_expr() && binop.right.is_name_expr() {
                        continue;
                    }
                    
                    if binop.left.is_name_expr() {
                        let r;
                        if binop.right.is_constant_expr() {
                            r = match_constant(&binop.right.as_constant_expr().unwrap().value);
                        } else {
                            // right subtree is binop
                            r = evaluate_bin_op_type(binop.right.as_bin_op_expr().unwrap());
                        }
                        l_heuristic = Heuristic { 
                            typename: r.unwrap(), 
                            score: f64::INFINITY 
                        };
                        let left_name = binop.left.as_name_expr().unwrap().id.as_str();
                        if !vars.contains_key(left_name) {
                            vars.insert(left_name, Vec::new());
                        }
                        if let Some(vec) = vars.get_mut(left_name) {
                            vec.push(l_heuristic);
                        }

                    } else if binop.right.is_name_expr() {
                        let l;
                        if binop.left.is_constant_expr() {
                            l = match_constant(&binop.right.as_constant_expr().unwrap().value);
                        } else {
                            // left subtree is binop
                            l = evaluate_bin_op_type(binop.right.as_bin_op_expr().unwrap());
                        }
                        r_heuristic = Heuristic { 
                            typename: l.unwrap(), 
                            score: f64::INFINITY 
                        };
                        let right_name = binop.left.as_name_expr().unwrap().id.as_str();
                        if !vars.contains_key(right_name) {
                            vars.insert(right_name, Vec::new());
                        }
                        if let Some(vec) = vars.get_mut(right_name) {
                            vec.push(r_heuristic);
                        }
                    }
                }
            },
            _ => {}
        }
    }
    vars
}

fn evaluate_bin_op_type(binop: &ExprBinOp) -> Option<ConstantType> {
    let left;
    let right;
    if binop.left.is_constant_expr() {
        let l = binop.left.as_constant_expr().unwrap();
        left = match_constant(&l.value);
    } else {
        left = evaluate_bin_op_type(binop.left.as_bin_op_expr().unwrap());
    }
    if binop.right.is_constant_expr() {
        let r = binop.right.as_constant_expr().unwrap();
        right = match_constant(&r.value);
    } else {
        right = evaluate_bin_op_type(binop.right.as_bin_op_expr().unwrap());
    }
    if left.is_none() && right.is_none() {
        return None;
    }
    
    match 
    (
        left.unwrap_or(ConstantType::None), right.unwrap_or(ConstantType::None)
    ) {
        (ConstantType::Float, _) => Some(ConstantType::Float),
        (_, ConstantType::Float) => Some(ConstantType::Float),
        (ConstantType::Int, ConstantType::Int) => Some(ConstantType::Int),
        _ => return Some(ConstantType::None)
    }
}

fn match_constant(constant: &Constant) -> Option<ConstantType> {
    match constant {
        Constant::Float(..) => return Some(ConstantType::Float),
        Constant::Int(..) => return Some(ConstantType::Int),
        Constant::Str(..) => return Some(ConstantType::Str),
        Constant::Bytes(..) => return Some(ConstantType::Bytes),
        Constant::Bool(..) => return Some(ConstantType::Bool),
        Constant::Tuple(..) => return Some(ConstantType::Tuple),
        Constant::Complex {..} => return Some(ConstantType::Complex),
        Constant::Ellipsis => return Some(ConstantType::Ellipsis),
        _ => return None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /**
     * Applying addition rule to binop x + 4.0 leads to heuristic map
     * { "x": [ Heuristic { Float, f64.INF } ] }
     */
    fn test_binop_with_float() {
        
    }
}