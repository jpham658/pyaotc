use crate::codegen::error::{BackendError, IRGenResult};
use crate::compiler::Compiler;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::{AddressSpace, IntPredicate};

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
                    // TODO: Get this to print True for 1, False for 0
                    string_format.push_str("%d ");
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
 * ChatGPT bc I just need a working print function atm
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

    // Create the entry block
    let entry_block = compiler.context.append_basic_block(function, "entry");
    compiler.builder.position_at_end(entry_block);

    // Retrieve the function parameter (the pointer to the Any struct)
    let any_container = function.get_nth_param(0).unwrap().into_pointer_value();

    let i8_type = compiler.context.i8_type();

    // Get the tag field (index 0)
    let tag_ptr = compiler
        .builder
        .build_struct_gep(any_container, 0, "tag_ptr")
        .expect("Error: Could not get tag pointer.");
    let tag_value = compiler
        .builder
        .build_load(tag_ptr, "tag")
        .expect("Error: Could not load tag field.")
        .into_int_value();

    // Create blocks for the tag cases and a merge block
    let block_match_0 = compiler.context.append_basic_block(function, "match_tag_0");
    let block_test_1 = compiler.context.append_basic_block(function, "test_tag_1");
    let block_match_1 = compiler.context.append_basic_block(function, "match_tag_1");
    let block_test_2 = compiler.context.append_basic_block(function, "test_tag_2");
    let block_match_2 = compiler.context.append_basic_block(function, "match_tag_2");
    let block_merge = compiler.context.append_basic_block(function, "merge");

    // Compare the tag and branch to the appropriate block
    let tag_is_0 = compiler
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            tag_value,
            i8_type.const_int(0, false),
            "tag_is_0",
        )
        .unwrap();
    let tag_is_1 = compiler
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            tag_value,
            i8_type.const_int(1, false),
            "tag_is_1",
        )
        .unwrap();
    let tag_is_2 = compiler
        .builder
        .build_int_compare(
            IntPredicate::EQ,
            tag_value,
            i8_type.const_int(2, false),
            "tag_is_2",
        )
        .unwrap();

    let _ = compiler
        .builder
        .build_conditional_branch(tag_is_0, block_match_0, block_test_1);
    let _ = compiler.builder.position_at_end(block_test_1);
    let _ = compiler
        .builder
        .build_conditional_branch(tag_is_1, block_match_1, block_test_2);
    let _ = compiler.builder.position_at_end(block_test_2);
    let _ = compiler
        .builder
        .build_conditional_branch(tag_is_2, block_match_2, block_merge);

    // Logic for tag = 0 (i8)
    compiler.builder.position_at_end(block_match_0);
    let any_as_bool_ptr = compiler.builder
        .build_bit_cast(any_container, compiler.any_bool_type.ptr_type(AddressSpace::default()), "any_as_bool_ptr")
        .unwrap()
        .into_pointer_value();
    let any_bool_value_ptr = compiler
        .builder
        .build_struct_gep(any_as_bool_ptr, 1, "any_bool_value_ptr")
        .expect("Error: Could not get value pointer.");
    let any_bool_value = compiler
        .builder
        .build_load(any_bool_value_ptr, "any_bool_value")
        .expect("Error: Could not load value as i8.")
        .into_int_value();
    let _ = compiler
        .builder
        .build_call(
            print_f,
            &[
                int_format_str.as_pointer_value().into(),
                BasicMetadataValueEnum::IntValue(any_bool_value),
            ],
            "print_call",
        )
        .expect("Could not call printf.");
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    // Logic for tag = 1 (i64)
    compiler.builder.position_at_end(block_match_1);
    let any_i64_ptr = compiler
        .builder
        .build_bit_cast(
            any_container,
            compiler.any_int_type.ptr_type(AddressSpace::default()),
            "any_i64_ptr",
        )
        .unwrap()
        .into_pointer_value();
    let any_i64_value_ptr = compiler
        .builder
        .build_struct_gep(any_i64_ptr, 1, "any_i64_value_ptr")
        .expect("Error: Could not load value as i64.");
    let any_i64_value = compiler
        .builder
        .build_load(any_i64_value_ptr, "any_i64_value")
        .expect("Error: Could not load value as i64.")
        .into_int_value();
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
    // Call printf or handle value_as_i64
    let _ = compiler.builder.build_unconditional_branch(block_merge);

    // Logic for tag = 2 (f64)
    compiler.builder.position_at_end(block_match_2);
    let any_f64_ptr = compiler
        .builder
        .build_bit_cast(
            any_container,
            compiler.any_float_type.ptr_type(AddressSpace::default()),
            "any_f64_ptr",
        )
        .unwrap()
        .into_pointer_value();
    let any_f64_value_ptr = compiler
        .builder
        .build_struct_gep(any_f64_ptr, 1, "any_f64_value_ptr")
        .expect("Error: Could not load value as f64.");
    let any_f64_value = compiler
        .builder
        .build_load(any_f64_value_ptr, "any_f64_value")
        .expect("Error: Could not load value as f64.")
        .into_float_value();
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
