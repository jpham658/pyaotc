use inkwell::{
    values::{BasicMetadataValueEnum, FloatValue, FunctionValue, PointerValue},
    AddressSpace,
};

use crate::{
    codegen::any_class_utils::{any_is_float, any_is_int, cast_any_to_struct, get_tag, get_value},
    compiler::Compiler,
};

/**
 * Generic add function that takes two Any values and returns the addition
 * of the two.
 */
pub fn build_g_add<'a>(compiler: &Compiler<'a>) -> FunctionValue<'a> {
    let main_entry = compiler
        .builder
        .get_insert_block()
        .expect("Builder isn't mapped to a basic block?");

    let any_type_ptr = compiler.any_type.ptr_type(AddressSpace::default());
    let g_add_fn_type = any_type_ptr.fn_type(&[any_type_ptr.into(), any_type_ptr.into()], false);
    let f64_type = compiler.context.f64_type();

    let function = compiler.module.add_function("g_add", g_add_fn_type, None);

    let entry_block = compiler.context.append_basic_block(function, "entry");
    compiler.builder.position_at_end(entry_block);

    let left_any = function.get_nth_param(0).unwrap().into_pointer_value();
    let right_any = function.get_nth_param(1).unwrap().into_pointer_value();

    let left_tag_value = get_tag(left_any, compiler);
    let right_tag_value = get_tag(right_any, compiler);

    let left_is_int = any_is_int(compiler, left_tag_value);
    let left_is_float = any_is_float(compiler, left_tag_value);
    let right_is_int = any_is_int(compiler, right_tag_value);
    let right_is_float = any_is_float(compiler, right_tag_value);

    let malloc_fn = compiler
        .module
        .get_function("malloc")
        .expect("malloc has not been declared.");
    let malloc_call = compiler
        .builder
        .build_call(malloc_fn, &[
            BasicMetadataValueEnum::IntValue(
                compiler.context.i64_type().const_int(16, false)
            )
        ], "malloc_call")
        .expect("Could not allocate memory for Any struct on heap.")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    let sum_ptr = compiler
        .builder
        .build_bit_cast(malloc_call, compiler.any_type.ptr_type(AddressSpace::default()), "sum_ptr")
        .expect("Could not convert malloc to Any type pointer.")
        .into_pointer_value();

    // Conditions
    // In form [left_type]_[right_type]
    let int_int = compiler
        .builder
        .build_and(left_is_int, right_is_int, "int_int")
        .expect("Could not build 'and' condition for int_int.");
    let float_int = compiler
        .builder
        .build_and(left_is_float, right_is_int, "float_int")
        .expect("Could not build 'and' condition for float_int.");
    let int_float = compiler
        .builder
        .build_and(left_is_int, right_is_float, "int_float")
        .expect("Could not build 'and' condition for int_float.");
    let float_float = compiler
        .builder
        .build_and(left_is_float, right_is_float, "float_float")
        .expect("Could not build 'and' condition for float_float.");

    // TODO: Throw some sort of error here if we have incompatible types
    let block_int_int = compiler.context.append_basic_block(function, "int_int");
    let block_test_float_int = compiler
        .context
        .append_basic_block(function, "test_float_int");
    let block_float_int = compiler.context.append_basic_block(function, "float_int");
    let block_test_int_float = compiler
        .context
        .append_basic_block(function, "test_int_float");
    let block_int_float = compiler.context.append_basic_block(function, "int_float");
    let block_test_float_float = compiler
        .context
        .append_basic_block(function, "test_float_float");
    let block_float_float = compiler.context.append_basic_block(function, "float_float");
    let block_merge = compiler.context.append_basic_block(function, "merge");

    // Declare conditionals
    let _ = compiler
        .builder
        .build_conditional_branch(int_int, block_int_int, block_test_float_int);
    let _ = compiler.builder.position_at_end(block_test_float_int);
    let _ =
        compiler
            .builder
            .build_conditional_branch(float_int, block_float_int, block_test_int_float);
    let _ = compiler.builder.position_at_end(block_test_int_float);
    let _ = compiler.builder.build_conditional_branch(
        int_float,
        block_int_float,
        block_test_float_float,
    );
    let _ = compiler.builder.position_at_end(block_test_float_float);
    let _ = compiler
        .builder
        .build_conditional_branch(float_float, block_float_float, block_merge);

    // Block int_int
    let _ = compiler.builder.position_at_end(block_int_int);
    let left_as_int_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
    let right_as_int_ptr = cast_any_to_struct(right_any, compiler.any_int_type, compiler);
    let left_val = get_value(left_as_int_ptr, compiler).into_int_value();
    let right_val = get_value(right_as_int_ptr, compiler).into_int_value();

    let sum_tag = compiler.context.i8_type().const_int(1, false);
    let sum_tag_ptr = compiler
        .builder
        .build_struct_gep(sum_ptr, 0, "sum_tag_ptr")
        .expect("Error: Could not get Any container tag.");
    let _ = compiler.builder.build_store(sum_tag_ptr, sum_tag);

    let sum_ptr_as_int = cast_any_to_struct(sum_ptr, compiler.any_int_type, compiler);
    let int_sum_val = compiler
        .builder
        .build_int_nsw_add(left_val, right_val, "int_sum_val")
        .expect("Could not perform integer addition.");
    let sum_val_ptr = compiler
        .builder
        .build_struct_gep(sum_ptr_as_int, 1, "sum_tag_ptr")
        .expect("Error: Could not get Any container tag.");
    let _ = compiler.builder.build_store(sum_val_ptr, int_sum_val);
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    // Block float_int
    let _ = compiler.builder.position_at_end(block_float_int);
    let left_as_float_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
    let right_as_int_ptr = cast_any_to_struct(right_any, compiler.any_int_type, compiler);
    let left_val = get_value(left_as_float_ptr, compiler).into_float_value();
    let right_val = get_value(right_as_int_ptr, compiler).into_int_value();
    let right_val_as_float = compiler
        .builder
        .build_signed_int_to_float(right_val, f64_type, "right_val_as_float")
        .expect("Could not cast right operand to float.");
    build_fadd(left_val, right_val_as_float, sum_ptr, compiler);
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    // Block int_float
    let _ = compiler.builder.position_at_end(block_int_float);
    let left_as_int_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
    let right_as_float_ptr = cast_any_to_struct(right_any, compiler.any_float_type, compiler);
    let left_val = get_value(left_as_int_ptr, compiler).into_int_value();
    let left_val_as_float = compiler
        .builder
        .build_signed_int_to_float(left_val, f64_type, "left_val_as_float")
        .expect("Could not cast left operand to float.");
    let right_val = get_value(right_as_float_ptr, compiler).into_float_value();
    build_fadd(left_val_as_float, right_val, sum_ptr, compiler);
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    // Block float_float
    let _ = compiler.builder.position_at_end(block_float_float);
    let left_as_float_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
    let right_as_float_ptr = cast_any_to_struct(right_any, compiler.any_float_type, compiler);
    let left_val = get_value(left_as_float_ptr, compiler).into_float_value();
    let right_val = get_value(right_as_float_ptr, compiler).into_float_value();
    build_fadd(left_val, right_val, sum_ptr, compiler);
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    let _ = compiler.builder.position_at_end(block_merge);
    let _ = compiler.builder.build_return(Some(&sum_ptr));

    // reset back to normal!
    compiler.builder.position_at_end(main_entry);

    function
}

fn build_fadd<'a>(
    left_val: FloatValue<'a>,
    right_val: FloatValue<'a>,
    sum_ptr: PointerValue<'a>,
    compiler: &Compiler<'a>,
) {
    let sum_tag = compiler.context.i8_type().const_int(2, false);
    let sum_tag_ptr = compiler
        .builder
        .build_struct_gep(sum_ptr, 0, "sum_tag_ptr")
        .expect("Error: Could not get Any container tag.");
    let _ = compiler.builder.build_store(sum_tag_ptr, sum_tag);

    let sum_ptr_as_float = cast_any_to_struct(sum_ptr, compiler.any_float_type, compiler);
    let float_sum_val = compiler
        .builder
        .build_float_add(left_val, right_val, &"fadd")
        .expect("Could not perform float addition.");
    let sum_val_ptr = compiler
        .builder
        .build_struct_gep(sum_ptr_as_float, 1, "sum_tag_ptr")
        .expect("Error: Could not get Any container tag.");
    let _ = compiler.builder.build_store(sum_val_ptr, float_sum_val);
}
