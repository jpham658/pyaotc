use inkwell::{values::PointerValue, AddressSpace};
use crate::compiler::Compiler;

pub trait ToAnyType {
    /**
     * Any type implementing this trait will be able to create
     * a generic Any version of itself.
     * Returns a pointer to the struct representing the target 
     * value's generic self.
     */
    fn to_any_type<'a>(&self, compiler: &Compiler<'a>) -> PointerValue<'a>;
}

impl ToAnyType for i8 {
    fn to_any_type<'a>(&self, compiler: &Compiler<'a>) -> PointerValue<'a> {
        let i8_type = compiler.context.i8_type();

        let any_container = compiler.builder
            .build_alloca(compiler.any_type, "any_int")
            .expect("Error: Could not allocate memory for Any type.");

        let tag = i8_type.const_int(0, false);
        let value = i8_type.const_int(*self as u64, false);

        let tag_ptr = compiler.builder
            .build_struct_gep(any_container, 0, "tag_ptr")
            .expect("Error: Could not get Any container tag.");
        let _ = compiler.builder
            .build_store(tag_ptr, tag);

        let any_bool_type_ptr = compiler.any_bool_type.ptr_type(AddressSpace::default());
        let casted_container = compiler.builder
            .build_bit_cast(any_container, any_bool_type_ptr, "container_as_bool")
            .expect("Error: Could not cast Any container pointer to boolean variation.");

        let value_ptr = compiler.builder
            .build_struct_gep(casted_container.into_pointer_value(), 1, "value_ptr")
            .expect("Error: Could not get pointer to AnyBool container value.");
        
        let _ = compiler.builder
            .build_store(value_ptr, value);

        any_container
    }
}

impl ToAnyType for i64 {
    fn to_any_type<'a>(&self, compiler: &Compiler<'a>) -> PointerValue<'a> {
        let i8_type = compiler.context.i8_type();
        let i64_type = compiler.context.i64_type();
        
        let any_container = compiler.builder
            .build_alloca(compiler.any_type, "any_int")
            .expect("Error: Could not allocate memory for Any type.");
        
        let tag = i8_type.const_int(1, false);
        let value = i64_type.const_int(*self as u64, false);

        let tag_ptr = compiler.builder
            .build_struct_gep(any_container, 0, "tag_ptr")
            .expect("Error: Could not get Any container tag.");
        let _ = compiler.builder
            .build_store(tag_ptr, tag);

        let any_int_type_ptr = compiler.any_int_type.ptr_type(AddressSpace::default());
        let casted_container = compiler.builder
            .build_bit_cast(any_container, any_int_type_ptr, "container_as_int")
            .expect("Error: Could not cast Any container pointer to integer variation.");

        let value_ptr = compiler.builder
            .build_struct_gep(casted_container.into_pointer_value(), 1, "value_ptr")
            .expect("Error: Could not get pointer to AnyInt container value.");
        
        let _ = compiler.builder
            .build_store(value_ptr, value);

        any_container
    }
}

impl ToAnyType for f64 {
    fn to_any_type<'a>(&self, compiler: &Compiler<'a>) -> PointerValue<'a> {
        let i8_type = compiler.context.i8_type();
        let f64_type = compiler.context.f64_type();

        let any_container = compiler.builder
            .build_alloca(compiler.any_type, "any_float")
            .expect("Error: Could not allocate memory for Any type.");
        
        let tag = i8_type.const_int(2, false);
        let value = f64_type.const_float(*self);

        let tag_ptr = compiler.builder
            .build_struct_gep(any_container, 0, "tag_ptr")
            .expect("Error: Could not get Any container tag.");
        let _ = compiler.builder
            .build_store(tag_ptr, tag);

        let any_float_type_ptr = compiler.any_float_type.ptr_type(AddressSpace::default());
        let casted_container = compiler.builder
            .build_bit_cast(any_container, any_float_type_ptr, "container_as_float")
            .expect("Error: Could not cast Any container pointer to float variation.");

        let value_ptr = compiler.builder
            .build_struct_gep(casted_container.into_pointer_value(), 1, "value_ptr")
            .expect("Error: Could not get pointer to AnyFloat container value.");
        
        let _ = compiler.builder
            .build_store(value_ptr, value);

        any_container
    }
}
