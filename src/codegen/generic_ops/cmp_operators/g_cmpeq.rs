use inkwell::{
    values::{FloatValue, FunctionValue, IntValue},
    AddressSpace,
};
use rustpython_parser::ast::{CmpOp, CmpOpEq};

use crate::{
    codegen::any_class_utils::{
        any_is_bool, any_is_float, any_is_int, cast_any_to_struct, get_tag, get_value,
    },
    compiler::Compiler,
};

use super::build_generic_cmp_op::BuildGenericOp;

impl BuildGenericOp for CmpOpEq {
    fn build_generic_op<'a>(compiler: &Compiler<'a>) -> Option<FunctionValue<'a>> {
        let main_entry = compiler.builder.get_insert_block().unwrap();
        let any_type_ptr = compiler.any_type.ptr_type(AddressSpace::default());
        let g_eq_fn_type = any_type_ptr.fn_type(&[any_type_ptr.into(), any_type_ptr.into()], false);
        let f64_type = compiler.context.f64_type();

        let fn_name = format!("{:?}", CmpOp::Eq);
        let function = compiler.module.add_function(&fn_name, g_eq_fn_type, None);

        let entry_block = compiler.context.append_basic_block(function, "entry");
        compiler.builder.position_at_end(entry_block);

        let left_any = function.get_nth_param(0).unwrap().into_pointer_value();
        let right_any = function.get_nth_param(1).unwrap().into_pointer_value();

        let left_tag_value = get_tag(left_any, compiler);
        let right_tag_value = get_tag(right_any, compiler);

        // TODO: Ask - should I just return bool here instead of Any...?
        let res_ptr = compiler
            .builder
            .build_malloc(compiler.any_type, "malloc")
            .expect("Could not allocate heap memory.");
        // Res is always boolean
        let res_tag = compiler.context.i8_type().const_int(0, false);
        let res_tag_ptr = compiler
            .builder
            .build_struct_gep(res_ptr, 0, "sum_tag_ptr")
            .expect("Error: Could not get Any container tag.");
        let _ = compiler.builder.build_store(res_tag_ptr, res_tag);
        let res_ptr_as_bool = cast_any_to_struct(res_ptr, compiler.any_bool_type, compiler);

        // Conditions
        let same_type = compiler
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                left_tag_value,
                right_tag_value,
                "",
            )
            .unwrap();

        let left_is_int = any_is_int(compiler, left_tag_value);
        let left_is_float = any_is_float(compiler, left_tag_value);
        let left_is_bool = any_is_bool(compiler, left_tag_value);
        let right_is_int = any_is_int(compiler, right_tag_value);
        let right_is_float = any_is_float(compiler, right_tag_value);
        let right_is_bool = any_is_bool(compiler, right_tag_value);

        let int_int = compiler
            .builder
            .build_and(left_is_int, right_is_int, "int_int")
            .expect("Could not build 'and' condition for int_int.");
        let float_float = compiler
            .builder
            .build_and(left_is_float, right_is_float, "float_float")
            .expect("Could not build 'and' condition for float_float.");
        let bool_bool = compiler
            .builder
            .build_and(left_is_bool, right_is_bool, "bool_bool")
            .expect("Could not build 'and' condition for bool_bool.");

        let block_same_type = compiler.context.append_basic_block(function, "same_type");
        let block_int_int = compiler.context.append_basic_block(function, "int_int");
        let block_test_float_float = compiler
            .context
            .append_basic_block(function, "test_float_float");
        let block_float_float = compiler.context.append_basic_block(function, "float_float");
        let block_test_bool_bool = compiler
            .context
            .append_basic_block(function, "test_bool_bool");
        let block_bool_bool = compiler.context.append_basic_block(function, "bool_bool");

        let block_test_left_int = compiler
            .context
            .append_basic_block(function, "test_left_int");
        let block_test_right_int = compiler
            .context
            .append_basic_block(function, "test_right_int");
        let block_left_int = compiler.context.append_basic_block(function, "left_int");
        let block_right_int = compiler.context.append_basic_block(function, "right_int");

        let block_true = compiler.context.append_basic_block(function, "true");
        let block_false = compiler.context.append_basic_block(function, "false");
        let block_merge = compiler.context.append_basic_block(function, "merge");
        let block_unreachable = compiler.context.append_basic_block(function, "unreachable");

        // Declare conditionals
        let _ = compiler.builder.build_conditional_branch(
            same_type,
            block_same_type,
            block_test_left_int,
        );

        let _ = compiler.builder.position_at_end(block_same_type);
        let _ = compiler.builder.build_conditional_branch(
            int_int,
            block_int_int,
            block_test_float_float,
        );
        let _ = compiler.builder.position_at_end(block_test_float_float);
        let _ = compiler.builder.build_conditional_branch(
            float_float,
            block_float_float,
            block_test_bool_bool,
        );
        let _ = compiler.builder.position_at_end(block_test_bool_bool);
        let _ = compiler.builder.build_conditional_branch(
            bool_bool,
            block_bool_bool,
            block_unreachable,
        );
        let _ = compiler.builder.position_at_end(block_test_left_int);
        let _ = compiler.builder.build_conditional_branch(
            right_is_float,
            block_left_int,
            block_test_right_int,
        );
        let _ = compiler.builder.position_at_end(block_test_right_int);
        let _ =
            compiler
                .builder
                .build_conditional_branch(left_is_float, block_right_int, block_false);

        // Block int_int
        let _ = compiler.builder.position_at_end(block_int_int);
        let left_as_int_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
        let right_as_int_ptr = cast_any_to_struct(right_any, compiler.any_int_type, compiler);
        let left_val = get_value(left_as_int_ptr, compiler).into_int_value();
        let right_val = get_value(right_as_int_ptr, compiler).into_int_value();
        let cmp = compiler
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, left_val, right_val, "")
            .expect("Could not compare int values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block float_float
        let _ = compiler.builder.position_at_end(block_float_float);
        let left_as_float_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
        let right_as_float_ptr = cast_any_to_struct(right_any, compiler.any_float_type, compiler);
        let left_val = get_value(left_as_float_ptr, compiler).into_float_value();
        let right_val = get_value(right_as_float_ptr, compiler).into_float_value();
        let cmp = compiler
            .builder
            .build_float_compare(inkwell::FloatPredicate::OEQ, left_val, right_val, "")
            .expect("Could not compare float values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block bool_bool
        let _ = compiler.builder.position_at_end(block_bool_bool);
        let left_as_bool_ptr = cast_any_to_struct(left_any, compiler.any_bool_type, compiler);
        let right_as_bool_ptr = cast_any_to_struct(right_any, compiler.any_bool_type, compiler);
        let left_val = get_value(left_as_bool_ptr, compiler).into_int_value();
        let right_val = get_value(right_as_bool_ptr, compiler).into_int_value();
        let cmp = compiler
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, left_val, right_val, "")
            .expect("Could not compare bool values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block left_is_int
        let _ = compiler.builder.position_at_end(block_left_int);
        let left_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
        let left_val = get_value(left_ptr, compiler).into_int_value();
        let left_as_float = compiler
            .builder
            .build_signed_int_to_float(left_val, f64_type, "")
            .expect("Could not cast left value to float.");
        let right_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
        let right_val = get_value(right_ptr, compiler).into_float_value();
        let cmp = compiler
            .builder
            .build_float_compare(inkwell::FloatPredicate::OEQ, left_as_float, right_val, "")
            .expect("Could not compare float values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block right_is_int
        let _ = compiler.builder.position_at_end(block_right_int);
        let left_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
        let left_val = get_value(left_ptr, compiler).into_float_value();
        let right_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
        let right_val = get_value(right_ptr, compiler).into_int_value();
        let right_as_float = compiler
            .builder
            .build_signed_int_to_float(right_val, f64_type, "")
            .expect("Could not cast right value to float.");
        let cmp = compiler
            .builder
            .build_float_compare(inkwell::FloatPredicate::OEQ, left_val, right_as_float, "")
            .expect("Could not compare float values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block true
        let _ = compiler.builder.position_at_end(block_true);
        let truth_val = compiler.context.i8_type().const_int(u64::from(true), false);
        let res_val_ptr = compiler
            .builder
            .build_struct_gep(res_ptr_as_bool, 1, "res_val_ptr")
            .expect("Error: Could not get Any container value.");
        let _ = compiler.builder.build_store(res_val_ptr, truth_val);
        let _ = compiler.builder.build_unconditional_branch(block_merge);

        // Block false
        let _ = compiler.builder.position_at_end(block_false);
        let false_val = compiler
            .context
            .i8_type()
            .const_int(u64::from(false), false);
        let res_val_ptr = compiler
            .builder
            .build_struct_gep(res_ptr_as_bool, 1, "res_val_ptr")
            .expect("Error: Could not get Any container value.");
        let _ = compiler.builder.build_store(res_val_ptr, false_val);
        let _ = compiler.builder.build_unconditional_branch(block_merge);

        // Unreachable block
        let _ = compiler.builder.position_at_end(block_unreachable);
        let _ = compiler.builder.build_unreachable();

        // Merge block
        let _ = compiler.builder.position_at_end(block_merge);
        let _ = compiler.builder.build_return(Some(&res_ptr));

        // reset back to normal!
        compiler.builder.position_at_end(main_entry);

        Some(function)
    }
}
