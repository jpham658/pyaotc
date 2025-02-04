use inkwell::{
    types::StructType,
    values::{BasicValueEnum, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};

/**
 * TODO: GET RID OF THIS!!! IT'S OBSOLETE!!!
 */

use crate::compiler::Compiler;

/**
 * Get tag of a given Any struct.
 */
pub fn get_tag<'a>(any_value_ptr: PointerValue<'a>, compiler: &Compiler<'a>) -> IntValue<'a> {
    let tag_ptr: inkwell::values::PointerValue<'a> = compiler
        .builder
        .build_struct_gep(any_value_ptr, 0, "tag_ptr")
        .expect("Error: Could not get tag pointer.");
    compiler
        .builder
        .build_load(tag_ptr, "tag")
        .expect("Error: Could not load tag field.")
        .into_int_value()
}

/**
 * Get value of a given Any struct.
 */
pub fn get_value<'a>(
    any_value_ptr: PointerValue<'a>,
    compiler: &Compiler<'a>,
) -> BasicValueEnum<'a> {
    let value_ptr: inkwell::values::PointerValue<'a> = compiler
        .builder
        .build_struct_gep(any_value_ptr, 1, "value_ptr")
        .expect("Error: Could not get value pointer.");
    compiler
        .builder
        .build_load(value_ptr, "value")
        .expect("Error: Could not load value field.")
}

pub fn any_is_bool<'a>(compiler: &Compiler<'a>, tag: IntValue<'a>) -> IntValue<'a> {
    compiler
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            tag,
            compiler.context.i8_type().const_int(0, false),
            "tag_is_0",
        )
        .expect("Could not compare if tag is 0.")
}

pub fn any_is_int<'a>(compiler: &Compiler<'a>, tag: IntValue<'a>) -> IntValue<'a> {
    compiler
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            tag,
            compiler.context.i8_type().const_int(1, false),
            "tag_is_1",
        )
        .expect("Could not compare if tag is 1.")
}

pub fn any_is_float<'a>(compiler: &Compiler<'a>, tag: IntValue<'a>) -> IntValue<'a> {
    compiler
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            tag,
            compiler.context.i8_type().const_int(2, false),
            "tag_is_2",
        )
        .expect("Could not compare if tag is 2.")
}

/**
 * Cast Any value to struct variation.
 */
pub fn cast_any_to_struct<'a>(
    any_value_ptr: PointerValue<'a>,
    new_type: StructType<'a>,
    compiler: &Compiler<'a>,
) -> PointerValue<'a> {
    compiler
        .builder
        .build_bit_cast(
            any_value_ptr,
            new_type.ptr_type(AddressSpace::default()),
            "",
        )
        .expect("Could not cast Any value to struct variant.")
        .into_pointer_value()
}
