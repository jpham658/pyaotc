use inkwell::values::FunctionValue;
use rustpython_parser::ast::{CmpOp, CmpOpEq};

use crate::compiler::Compiler;

pub fn build_generic_cmp_op<'a>(op: &CmpOp, compiler: &Compiler<'a>) -> Option<FunctionValue<'a>> {
    match op {
        CmpOp::Eq => CmpOpEq::build_generic_op(compiler),
        _ => None
    }
}

pub trait BuildGenericOp {
    fn build_generic_op<'a>(compiler: &Compiler<'a>) -> Option<FunctionValue<'a>>;
}