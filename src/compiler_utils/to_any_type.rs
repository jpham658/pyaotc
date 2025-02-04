use crate::compiler::Compiler;
use inkwell::values::{AnyValue, PointerValue};

pub trait ToAnyType {
    /**
     * Any type implementing this trait will be able to create
     * a generic Any version of itself.
     * Returns a pointer to the struct representing the target
     * value's generic self.
     */
    fn to_any_type<'a>(&self, compiler: &Compiler<'a>) -> PointerValue<'a>;
}

impl ToAnyType for i64 {
    fn to_any_type<'a>(&self, compiler: &Compiler<'a>) -> PointerValue<'a> {
        let new_int_fn = compiler
            .module
            .get_function("new_int")
            .expect("new_int has not been declared.");

        let int_val = compiler.context.i64_type().const_int(*self as u64, false);
        let int_obj = compiler
            .builder
            .build_call(new_int_fn, &[int_val.into()], "")
            .expect("Could not create int Any object.")
            .as_any_value_enum();

        int_obj.into_pointer_value()
    }
}

impl ToAnyType for f64 {
    fn to_any_type<'a>(&self, compiler: &Compiler<'a>) -> PointerValue<'a> {
        let new_float_fn = compiler
            .module
            .get_function("new_float")
            .expect("new_float has not been declared.");

        let float_val = compiler.context.f64_type().const_float(*self);
        let float_obj = compiler
            .builder
            .build_call(new_float_fn, &[float_val.into()], "")
            .expect("Could not create float Any object.")
            .as_any_value_enum();

        float_obj.into_pointer_value()
    }
}
