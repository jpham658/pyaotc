use inkwell::values::FunctionValue;
use rustpython_parser::ast::{UnaryOp, UnaryOpNot, UnaryOpUAdd, UnaryOpUSub};

use crate::compiler::Compiler;

pub fn build_generic_uop<'a>(op: &UnaryOp, compiler: &Compiler<'a>) -> Option<FunctionValue<'a> >{
    match op {
        // UnaryOp::Not => UnaryOpNot::build_generic_op(compiler),
        // UnaryOp::UAdd => UnaryOpUAdd::build_generic_op(compiler),
        // UnaryOp::USub => UnaryOpUSub::build_generic_op(compiler),
        _ => None
    }
}