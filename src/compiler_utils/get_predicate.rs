use inkwell::{IntPredicate, FloatPredicate};
use rustpython_parser::ast::CmpOp;

/**
 * Util to get float predicate for given boolean comparison.
 */
pub fn get_float_predicate(op: CmpOp) -> FloatPredicate {
    match op {
        CmpOp::Eq => FloatPredicate::OEQ,
        CmpOp::NotEq => FloatPredicate::ONE,
        CmpOp::Gt => FloatPredicate::OGT,
        CmpOp::GtE => FloatPredicate::OGE,
        CmpOp::Lt => FloatPredicate::OLT,
        CmpOp::LtE => FloatPredicate::OLE,
        CmpOp::In | CmpOp::Is | CmpOp::IsNot | CmpOp::NotIn => FloatPredicate::PredicateFalse // TODO: Fix how to handle this
    }
}

/**
 * Util to get int predicate for given boolean comparison.
 */
pub fn get_int_predicate(op: CmpOp) -> IntPredicate {
    match op {
        CmpOp::Eq => IntPredicate::EQ,
        CmpOp::NotEq => IntPredicate::NE,
        CmpOp::Gt => IntPredicate::SGT,
        CmpOp::GtE => IntPredicate::SGE,
        CmpOp::Lt => IntPredicate::SLT,
        CmpOp::LtE => IntPredicate::SLE,
        CmpOp::In | CmpOp::Is | CmpOp::IsNot | CmpOp::NotIn => IntPredicate::EQ // TODO: Fix how to handle this
    }
}