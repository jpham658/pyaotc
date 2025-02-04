use crate::codegen::any_class_utils::{
    any_is_bool, any_is_float, any_is_int, cast_any_to_struct, get_tag, get_value,
};
use crate::codegen::error::{BackendError, IRGenResult};
use crate::compiler::Compiler;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, FunctionValue};
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
                let bool_type = compiler.context.bool_type();
                if i.get_type() == bool_type {
                    let print_bool = if let Some(func) = compiler.module.get_function("print_bool") {
                        func
                    } else {
                        build_print_bool_fn(compiler)
                    };
                    let _ = compiler.builder.build_call(print_bool, &[
                        BasicMetadataValueEnum::IntValue(*i)
                    ], "");
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
 * Function that builds LLVM helper function for printing a boolean value.
 * Used when evaluating `print`
 */
fn build_print_bool_fn<'a>(compiler: &Compiler<'a>) -> FunctionValue<'a> {
    let main_entry = compiler
        .builder
        .get_insert_block()
        .expect("Builder isn't mapped to a basic block?");

    let print_f = compiler
        .module
        .get_function("printf")
        .expect("Could not find print function.");
    let true_format_str = compiler
        .builder
        .build_global_string_ptr("True ", "true_format_str")
        .expect("Error when creating string format.");
    let false_format_str = compiler
        .builder
        .build_global_string_ptr("False ", "false_format_str")
        .expect("Error when creating string format.");

    let bool_type = compiler.context.bool_type();
    let void_type = compiler.context.void_type();
    let print_bool_type = void_type.fn_type(&[bool_type.into()], false);
    let print_bool_fn = compiler
        .module
        .add_function("print_bool", print_bool_type, None);

    
    let entry = compiler.context.append_basic_block(print_bool_fn, "entry");
    let handle_print_true = compiler
    .context
    .append_basic_block(print_bool_fn, "handle_print_true");
    let handle_print_false = compiler
    .context
    .append_basic_block(print_bool_fn, "handle_print_false");
    let merge_block = compiler.context.append_basic_block(print_bool_fn, "merge");
    let _ = compiler.builder.position_at_end(entry);

    let bool_val = print_bool_fn.get_nth_param(0).unwrap().into_int_value();
    let truth_val = bool_type.const_int(u64::from(true), false);
    let is_true = compiler
        .builder
        .build_int_compare(inkwell::IntPredicate::EQ, bool_val, truth_val, "")
        .expect("Could not compare boolean values.");
    let _ =
        compiler
            .builder
            .build_conditional_branch(is_true, handle_print_true, handle_print_false);
    
    // handle_print_true
    let _ = compiler.builder.position_at_end(handle_print_true);
    let _  = compiler.builder.build_call(print_f, &[
        true_format_str.as_pointer_value().into()
    ], "");
    let _ = compiler.builder.build_unconditional_branch(merge_block);

    // handle_print_false
    let _ = compiler.builder.position_at_end(handle_print_false);
    let _  = compiler.builder.build_call(print_f, &[
        false_format_str.as_pointer_value().into()
    ], "");
    let _ = compiler.builder.build_unconditional_branch(merge_block);
    
    // merge
    let _ = compiler.builder.position_at_end(merge_block);
    let _ = compiler.builder.build_return(None);

    let _ = compiler.builder.position_at_end(main_entry);

    print_bool_fn
}