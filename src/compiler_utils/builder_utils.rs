use inkwell::{
    types::{AnyType, AnyTypeEnum, BasicType},
    values::{AnyValue, AnyValueEnum, FunctionValue, PointerValue},
    AddressSpace,
};
use rustpython_parser::ast::{Expr, ExprCall, Stmt};

use crate::{
    codegen::error::{BackendError, IRGenResult},
    compiler::Compiler,
};

/**
 * Helper to build a range call
 */
pub fn build_range_call<'ctx>(
    compiler: &mut Compiler<'ctx>,
    args: Vec<AnyValueEnum<'ctx>>,
) -> IRGenResult<'ctx> {
    if args.len() == 0 {
        return Err(BackendError {
            message: "Too few arguments given to range call.",
        });
    }

    if args.len() > 3 {
        return Err(BackendError {
            message: "Too many arguments given to range call.",
        });
    }

    for arg in &args {
        if !arg.is_int_value() {
            return Err(BackendError {
                message: "Arguments to range are incorrect.",
            });
        }
    }

    let start = if args.len() > 1 {
        args[0].into_int_value()
    } else {
        compiler.context.i64_type().const_int(0, false)
    };
    let stop = if args.len() > 1 {
        args[1].into_int_value()
    } else {
        args[0].into_int_value()
    };
    let step = if args.len() == 3 {
        args[2].into_int_value()
    } else {
        compiler.context.i64_type().const_int(1, false)
    };

    let range_fn = match compiler.module.get_function("create_range") {
        Some(func) => func,
        None => {
            let range_ptr_type = compiler
                .module
                .get_struct_type("struct.Range")
                .expect("Range is not declared...")
                .ptr_type(AddressSpace::default());
            let i64_type = compiler.context.i64_type();
            let arg_types = Vec::from([
                compiler
                    .convert_any_type_to_param_type(i64_type.as_any_type_enum())
                    .unwrap(),
                compiler
                    .convert_any_type_to_param_type(i64_type.as_any_type_enum())
                    .unwrap(),
                compiler
                    .convert_any_type_to_param_type(i64_type.as_any_type_enum())
                    .unwrap(),
            ]);
            let range_fn_type = range_ptr_type.fn_type(&arg_types, false);
            let _ = compiler
                .module
                .add_function("create_range", range_fn_type, None);
            compiler.module.get_function("create_range").unwrap()
        }
    };

    match compiler
        .builder
        .build_call(range_fn, &[start.into(), stop.into(), step.into()], "")
    {
        Ok(ir) => Ok(ir.as_any_value_enum()),
        Err(..) => Err(BackendError {
            message: "Could not build call.",
        }),
    }
}

/**
 * Helper to build for loop body.
 */
pub fn build_for_loop_body<'ctx>(
    compiler: &mut Compiler<'ctx>,
    iter_ptr: PointerValue<'ctx>,
    next_func: FunctionValue<'ctx>,
    target: &Expr,
    body: &[Stmt],
) {
    // get current from iterator
    // store that at the variable represented by target
}

fn initialise_global_variable<'ctx>(
    compiler: &mut Compiler<'ctx>,
    target_name: &str,
    value: &AnyValueEnum<'ctx>,
) -> IRGenResult<'ctx> {
    let typ = match value.get_type() {
        AnyTypeEnum::FloatType(f) => f.as_basic_type_enum(),
        AnyTypeEnum::IntType(i) => i.as_basic_type_enum(),
        AnyTypeEnum::PointerType(p) => p.as_basic_type_enum(),
        AnyTypeEnum::ArrayType(a) => a.as_basic_type_enum(),
        AnyTypeEnum::VectorType(v) => v.as_basic_type_enum(),
        AnyTypeEnum::StructType(s) => s.as_basic_type_enum(),
        _ => {
            return Err(BackendError {
                message: "Unsupported r-value type.",
            })
        }
    };

    let global = compiler
        .module
        .add_global(typ, Some(AddressSpace::default()), target_name);

    match value.get_type() {
        AnyTypeEnum::FloatType(..) => global.set_initializer(&value.into_float_value()),
        AnyTypeEnum::IntType(..) => global.set_initializer(&value.into_int_value()),
        AnyTypeEnum::PointerType(p) => {
            global.set_initializer(&p.const_null());
            store_value(compiler, &global.as_pointer_value(), value)?;
        }
        AnyTypeEnum::ArrayType(..) => global.set_initializer(&value.into_array_value()),
        AnyTypeEnum::VectorType(..) => global.set_initializer(&value.into_vector_value()),
        _ => {
            return Err(BackendError {
                message: "Unsupported r-value type.",
            })
        }
    }

    Ok(global.as_pointer_value().as_any_value_enum())
}

/**
 * Helper to build global assignment.
 * If typed_value is Some, then we need to generate a global variable for it
 * If generic_value is Some, then we need to generate a global variable for it
 */
pub fn handle_global_assignment<'ctx>(
    compiler: &mut Compiler<'ctx>,
    target_name: &str,
    typed_value: &Option<AnyValueEnum<'ctx>>,
    generic_value: &Option<AnyValueEnum<'ctx>>,
) -> IRGenResult<'ctx> {
    if typed_value.is_none() && generic_value.is_none() {
        return Err(BackendError {
            message: "Target must have at least one typed or generic value.",
        });
    }

    let resolved_var = compiler.sym_table.resolve_variable(target_name);

    match resolved_var {
        None => {
            // initialise global vars
            let global_ptr = if let Some(v) = typed_value {
                Some(initialise_global_variable(compiler, target_name, v)?)
            } else {
                None
            };

            let global_obj_ptr = if let Some(v) = generic_value {
                Some(initialise_global_variable(compiler, target_name, v)?)
            } else {
                None
            };

            compiler
                .sym_table
                .add_variable(target_name, global_ptr, global_obj_ptr);

            if let Some(ptr) = global_ptr {
                Ok(ptr)
            } else {
                Ok(global_obj_ptr.unwrap())
            }
        }
        Some((global_ptr, global_obj_ptr)) => {
            // return either global ptr, preferably typed global ptr
            if let Some(ptr) = global_ptr {
                Ok(ptr)
            } else {
                Ok(global_obj_ptr.unwrap())
            }
        }
    }

    // Declare a generic Object* version if applicable
}

/**
 * Helper to allocate memory on stack for given value
 */
pub fn allocate_variable<'ctx>(
    compiler: &mut Compiler<'ctx>,
    target_name: &str,
    typed_value: &AnyValueEnum<'ctx>,
) -> IRGenResult<'ctx> {
    let target_ptr = match typed_value.get_type() {
        AnyTypeEnum::FloatType(..) => compiler
            .builder
            .build_alloca(compiler.context.f64_type(), target_name),
        AnyTypeEnum::IntType(..) => {
            let int_type = typed_value.get_type().into_int_type();
            let alloc_type = if int_type == compiler.context.bool_type() {
                compiler.context.bool_type()
            } else {
                compiler.context.i64_type()
            };
            compiler.builder.build_alloca(alloc_type, target_name)
        }
        AnyTypeEnum::PointerType(..) => compiler
            .builder
            .build_alloca(typed_value.into_pointer_value().get_type(), target_name),
        _ => {
            return Err(BackendError {
                message: "Unsupported type for allocation",
            })
        }
    };

    match target_ptr {
        Ok(res) => Ok(res.as_any_value_enum()),
        Err(..) => Err(BackendError {
            message: "Could not allocate pointer.",
        }),
    }
}

/**
 * Helper to build store instruction
*/
pub fn store_value<'ctx>(
    compiler: &mut Compiler<'ctx>,
    target_ptr: &PointerValue<'ctx>,
    typed_value: &AnyValueEnum<'ctx>,
) -> IRGenResult<'ctx> {
    let store = match typed_value.get_type() {
        AnyTypeEnum::IntType(..) => compiler
            .builder
            .build_store(*target_ptr, typed_value.into_int_value()),
        AnyTypeEnum::FloatType(..) => compiler
            .builder
            .build_store(*target_ptr, typed_value.into_float_value()),
        AnyTypeEnum::PointerType(..) => compiler
            .builder
            .build_store(*target_ptr, typed_value.into_pointer_value()),
        AnyTypeEnum::StructType(..) => compiler
            .builder
            .build_store(*target_ptr, typed_value.into_struct_value()),
        _ => {
            return Err(BackendError {
                message: "Unsupported assignment type.",
            })
        }
    };

    match store {
        Ok(res) => Ok(res.as_any_value_enum()),
        Err(..) => Err(BackendError {
            message: "Unsupported assignment type.",
        }),
    }
}
