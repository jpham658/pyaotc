use inkwell::values::FunctionValue;

use crate::compiler::Compiler;

pub trait BuildGenericOp {
    fn build_generic_op<'a>(compiler: &Compiler<'a>) -> Option<FunctionValue<'a>>;
}