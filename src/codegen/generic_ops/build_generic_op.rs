use rustpython_parser::ast::{CmpOp, CmpOpEq, Operator, OperatorAdd, OperatorMult, OperatorSub, UnaryOp, UnaryOpNot, UnaryOpUAdd, UnaryOpUSub};

/**
 * Trait for every operator to return LLVM IR for
 * their generic counterpart.
 */
pub trait GenericOpIR {
    fn get_generic_op_ir() -> String;
}

/**
 * Function that gets LLVM IR for all generic comparison operators. 
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
 * Function that gets all LLVM IR for generic unary operators.
 */
pub fn get_generic_uops_ir() -> String {
    let ops = Vec::from([UnaryOp::USub, UnaryOp::UAdd, UnaryOp::Not]);
    ops.into_iter().map(|op| match op {
        UnaryOp::USub => UnaryOpUSub::get_generic_op_ir(),
        UnaryOp::UAdd => UnaryOpUAdd::get_generic_op_ir(),
        UnaryOp::Not => UnaryOpNot::get_generic_op_ir(),
        _ => r#""#.to_string()
    }).collect::<Vec<_>>()
    .join("\n")
}

/**
 * Function that gets all LLVM IR for generic binary operators.
 */
pub fn get_generic_ops_ir() -> String {
    let ops = Vec::from([Operator::Add, Operator::Sub, Operator::Mult]);
    ops.into_iter()
        .map(|op| match op {
            Operator::Add => OperatorAdd::get_generic_op_ir(),
            Operator::Sub => OperatorSub::get_generic_op_ir(),
            Operator::Mult => OperatorMult::get_generic_op_ir(),
            _ => r#""#.to_string(),
        })
        .collect::<Vec<_>>()
        .join("\n")
}
