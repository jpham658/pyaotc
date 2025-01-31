use crate::codegen::any_class_utils::{
    any_is_bool, any_is_float, any_is_int, cast_any_to_struct, get_tag, get_value,
};
use crate::codegen::error::{BackendError, IRGenResult};
use crate::compiler::Compiler;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;

pub fn print_fn<'a>(compiler: &Compiler<'a>, args: &[AnyValueEnum<'a>]) -> IRGenResult<'a> {
    let print_f = compiler
        .module
        .get_function("printf")
        .expect("Could not find print function.");
    let mut string_format = String::new();

    let mut llvm_args: Vec<BasicMetadataValueEnum<'a>> = Vec::new();

    for arg in args {
        match arg {
            AnyValueEnum::IntValue(i) => {
                // Handle boolean and integer values
                let i8_type = compiler.context.i8_type();
                if i.get_type() == i8_type {
                    let truth_val = i8_type.const_int(u64::from(true), false);
                    println!("{:?}", i);
                    if *i == truth_val {
                        string_format.push_str("True ");
                    } else {
                        string_format.push_str("False ");
                    }
                    llvm_args.push(BasicMetadataValueEnum::IntValue(*i));
                } else {
                    string_format.push_str("%d ");
                    llvm_args.push(BasicMetadataValueEnum::IntValue(*i));
                }
            }
            AnyValueEnum::FloatValue(f) => {
                string_format.push_str("%f ");
                llvm_args.push(BasicMetadataValueEnum::FloatValue(*f));
            }
            AnyValueEnum::PointerValue(ptr) => {
                // just prints address...
                string_format.push_str("%p ");
                llvm_args.push(BasicMetadataValueEnum::PointerValue(*ptr));
            }
            _ => {
                println!("{:?}", arg);
                return Err(BackendError {
                    message: "Unsupported argument type for print",
                });
            }
        }
    }

    string_format.push_str("\n");
    let global_string_format = compiler
        .builder
        .build_global_string_ptr(&string_format, "format_string")
        .expect("Error when creating string format.");

    llvm_args.insert(0, global_string_format.as_pointer_value().into());

    compiler
        .builder
        .build_call(print_f, &llvm_args, "print_call")
        .expect("Could not call printf.");

    Ok(compiler.context.i32_type().const_zero().as_any_value_enum())
}

/**
 * Function that builds the LLVM function for printing an Any value.
 */
pub fn build_print_any_fn<'a>(compiler: &Compiler<'a>) -> inkwell::values::FunctionValue<'a> {
    let main_entry = compiler
        .builder
        .get_insert_block()
        .expect("Builder isn't mapped to a basic block?");

    let print_f = compiler
        .module
        .get_function("printf")
        .expect("Could not find print function.");
    let any_type_ptr = compiler.any_type.ptr_type(AddressSpace::default());
    let void_type = compiler.context.void_type();
    let print_any_fn_type = void_type.fn_type(&[any_type_ptr.into()], false);

    let true_as_u64 = u64::from(true);
    let truth_val = compiler.context.i8_type().const_int(true_as_u64, false);

    let true_format_str = compiler
        .builder
        .build_global_string_ptr("True\n", "true_format_str")
        .expect("Error when creating string format.");
    let false_format_str = compiler
        .builder
        .build_global_string_ptr("False\n", "false_format_str")
        .expect("Error when creating string format.");
    let int_format_str = compiler
        .builder
        .build_global_string_ptr("%d\n", "int_format_str")
        .expect("Error when creating string format.");
    let f64_format_str = compiler
        .builder
        .build_global_string_ptr("%f\n", "f64_format_str")
        .expect("Error when creating string format.");

    let function = compiler
        .module
        .add_function("print_any", print_any_fn_type, None);

    let entry_block = compiler.context.append_basic_block(function, "entry");
    compiler.builder.position_at_end(entry_block);

    let any_container = function.get_nth_param(0).unwrap().into_pointer_value();

    let any_tag = get_tag(any_container, compiler);

    // Create blocks for the tag cases and a merge block
    let block_match_0 = compiler.context.append_basic_block(function, "match_tag_0");
    let block_test_1 = compiler.context.append_basic_block(function, "test_tag_1");
    let block_match_1 = compiler.context.append_basic_block(function, "match_tag_1");
    let block_test_2 = compiler.context.append_basic_block(function, "test_tag_2");
    let block_match_2 = compiler.context.append_basic_block(function, "match_tag_2");
    let block_merge = compiler.context.append_basic_block(function, "merge");

    // Compare the tag and branch to the appropriate block
    let any_is_bool = any_is_bool(compiler, any_tag);
    let any_is_int = any_is_int(compiler, any_tag);
    let any_is_float = any_is_float(compiler, any_tag);

    let _ = compiler
        .builder
        .build_conditional_branch(any_is_bool, block_match_0, block_test_1);
    let _ = compiler.builder.position_at_end(block_test_1);
    let _ = compiler
        .builder
        .build_conditional_branch(any_is_int, block_match_1, block_test_2);
    let _ = compiler.builder.position_at_end(block_test_2);
    let _ = compiler
        .builder
        .build_conditional_branch(any_is_float, block_match_2, block_merge);

    // Logic for tag = 0 (i8)
    compiler.builder.position_at_end(block_match_0);
    let any_as_bool_ptr = cast_any_to_struct(any_container, compiler.any_bool_type, compiler);
    let any_bool_value = get_value(any_as_bool_ptr, compiler).into_int_value();
    let is_bool = compiler
        .builder
        .build_int_compare(inkwell::IntPredicate::EQ, truth_val, any_bool_value, "")
        .unwrap();
    // TODO: Change to delegate at runtime
    if any_bool_value.eq(&truth_val) {
        let _ = compiler
            .builder
            .build_call(
                print_f,
                &[true_format_str.as_pointer_value().into()],
                "print_call",
            )
            .expect("Could not call printf.");
    } else {
        let _ = compiler
            .builder
            .build_call(
                print_f,
                &[false_format_str.as_pointer_value().into()],
                "print_call",
            )
            .expect("Could not call printf.");
    }
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    // Logic for tag = 1 (i64)
    compiler.builder.position_at_end(block_match_1);
    let any_i64_ptr = cast_any_to_struct(any_container, compiler.any_int_type, compiler);
    let any_i64_value = get_value(any_i64_ptr, compiler).into_int_value();
    let _ = compiler
        .builder
        .build_call(
            print_f,
            &[
                int_format_str.as_pointer_value().into(),
                BasicMetadataValueEnum::IntValue(any_i64_value),
            ],
            "print_call",
        )
        .expect("Could not call printf.");

    let _ = compiler.builder.build_unconditional_branch(block_merge);

    // Logic for tag = 2 (f64)
    compiler.builder.position_at_end(block_match_2);
    let any_f64_ptr = cast_any_to_struct(any_container, compiler.any_float_type, compiler);
    let any_f64_value = get_value(any_f64_ptr, compiler).into_float_value();
    let _ = compiler
        .builder
        .build_call(
            print_f,
            &[
                f64_format_str.as_pointer_value().into(),
                BasicMetadataValueEnum::FloatValue(any_f64_value),
            ],
            "print_call",
        )
        .expect("Could not call printf.");
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    let _ = compiler.builder.position_at_end(block_merge);
    let _ = compiler.builder.build_return(None);

    // reset back to normal!
    compiler.builder.position_at_end(main_entry);

    function
}
