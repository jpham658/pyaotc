use rustpython_parser::ast::{CmpOp, CmpOpEq, Operator, OperatorAdd};

/**
 * Trait for every operator to return LLVM IR for
 * their generic counterpart.
 */
pub trait GenericOpIR {
    fn get_generic_op_ir() -> String;
}

/**
 * Function that gets the string representing 
 * the LLVM IR for all comparison operators.
 */
pub fn get_generic_cmp_ops_ir() -> String {
    let cmp_ops = Vec::from([CmpOp::Eq]);
    cmp_ops
        .into_iter()
        .map(|op| match op {
            CmpOp::Eq => CmpOpEq::get_generic_op_ir(),
            _ => r#""#.to_string(),
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/**
 * Function that gets the string representing 
 * the LLVM IR for all operators.
 */
pub fn get_generic_ops_ir() -> String {
    let ops = Vec::from([Operator::Add]);
    ops.into_iter()
        .map(|op| match op {
            Operator::Add => OperatorAdd::get_generic_op_ir(),
            _ => r#""#.to_string(),
        })
        .collect::<Vec<_>>()
        .join("\n")
}
