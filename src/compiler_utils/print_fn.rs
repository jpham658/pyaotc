use crate::codegen::error::{BackendError, IRGenResult};
use crate::compiler::Compiler;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;

pub fn print_fn<'a>(compiler: &Compiler<'a>, args: &[AnyValueEnum<'a>]) -> IRGenResult<'a> {
    for arg in args {
        match arg {
            AnyValueEnum::IntValue(i) => {
                // Handle boolean and integer values
                let bool_type = compiler.context.bool_type();
                if i.get_type() == bool_type {
                    let print_bool = compiler
                        .module
                        .get_function("print_bool")
                        .expect("print_bool is not defined.");
                    let _ = compiler
                        .builder
                        .build_call(print_bool, &[BasicMetadataValueEnum::IntValue(*i)], "")
                        .expect("Could not call print_bool.");
                } else {
                    let print_int = compiler
                        .module
                        .get_function("print_int")
                        .expect("print_int is not defined.");
                    let _ = compiler
                        .builder
                        .build_call(print_int, &[BasicMetadataValueEnum::IntValue(*i)], "")
                        .expect("Could not call print_int.");
                }
            }
            AnyValueEnum::FloatValue(f) => {
                let print_float = compiler
                    .module
                    .get_function("print_float")
                    .expect("print_float is not defined.");
                let _ = compiler
                    .builder
                    .build_call(print_float, &[BasicMetadataValueEnum::FloatValue(*f)], "")
                    .expect("Could not call print_float.");
            }
            AnyValueEnum::PointerValue(ptr) => {
                let range_type = compiler.module.get_struct_type("struct.Range").unwrap();
                let list_type = compiler.module.get_struct_type("struct.List").unwrap();
                if ptr.get_type() == compiler.object_type.ptr_type(AddressSpace::default()) {
                    let print_obj = compiler
                        .module
                        .get_function("print_obj")
                        .expect("print_obj is not defined.");
                    let arg_num = compiler.context.i32_type().const_int(1, false);
                    let _ = compiler
                        .builder
                        .build_call(print_obj, &[
                            BasicMetadataValueEnum::IntValue(arg_num),
                            BasicMetadataValueEnum::PointerValue(*ptr)
                        ], "")
                        .expect("Could not call print_obj.");
                } else if ptr.get_type() == range_type.ptr_type(AddressSpace::default()) {
                    let print_range = compiler
                        .module
                        .get_function("print_range")
                        .expect("print_range is not defined.");
                    let _ = compiler
                        .builder
                        .build_call(print_range, &[
                            BasicMetadataValueEnum::PointerValue(*ptr)
                        ], "")
                        .expect("Could not call print_range.");
                } else if ptr.get_type() == list_type.ptr_type(AddressSpace::default()) {
                    let print_list = compiler
                        .module
                        .get_function("print_list")
                        .expect("print_list is not defined.");
                    let _ = compiler
                        .builder
                        .build_call(print_list, &[
                            BasicMetadataValueEnum::PointerValue(*ptr)
                        ], "")
                        .expect("Could not call print_list.");
                } else {
                    let print_str = compiler
                        .module
                        .get_function("print_str")
                        .expect("print_str is not defined.");
                    let _ = compiler
                        .builder
                        .build_call(print_str, &[BasicMetadataValueEnum::PointerValue(*ptr)], "")
                        .expect("Could not call print_str.");
                }
            }
            _ => {
                println!("{:?}", arg);
                return Err(BackendError {
                    message: "Unsupported argument type for print",
                });
            }
        }
    }

    let print_newline = compiler
        .module
        .get_function("print_newline")
        .expect("print_newline is not defined.");
    let _ = compiler
        .builder
        .build_call(print_newline, &[], "")
        .expect("Could not call print_newline.");

    Ok(compiler.context.i32_type().const_zero().as_any_value_enum())
}
