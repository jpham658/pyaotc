use crate::compiler::Compiler;
use crate::codegen::error::{BackendError, IRGenResult};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};

pub fn print_fn<'a>(compiler: &Compiler<'a>, args: &[AnyValueEnum]) -> IRGenResult<'a> {
    let print_f = compiler
        .module
        .get_function("printf")
        .expect("Could not find print function.");
    let mut string_format = String::new();
    let mut llvm_args: Vec<BasicMetadataValueEnum<'_>> = Vec::new();

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
            _ => {
                return Err(BackendError {
                    message: "Unsupported argument type for print",
                });
            }
        }
    }

    string_format.push_str("\n");
    let global_string_format = compiler
        .builder
        .build_global_string_ptr(
            &string_format,
            "format_string", 
        )
        .expect("Error when creating string format.");

    llvm_args.insert(0, global_string_format.as_pointer_value().into());

    compiler
        .builder
        .build_call(print_f, &llvm_args, "print_call")
        .expect("Could not call printf.");

    Ok(compiler.context.i32_type().const_zero().as_any_value_enum())
}
