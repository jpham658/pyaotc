use inkwell::values::FunctionValue;
use rustpython_parser::ast::{Operator, OperatorAdd};

use crate::{codegen::generic_ops::build_generic_op::BuildGenericOp, compiler::Compiler};

pub fn build_generic_op<'a>(op: &Operator, compiler: &Compiler<'a>) -> Option<FunctionValue<'a> >{
    match op {
        Operator::Add => OperatorAdd::build_generic_op(compiler),
        _ => None
    }
}