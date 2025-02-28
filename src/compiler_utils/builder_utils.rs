use inkwell::{
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, PointerType},
    values::{AnyValue, AnyValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};
use rustpython_ast::Ranged;
use rustpython_parser::ast::{Expr, Stmt};

use crate::{
    codegen::{
        error::{BackendError, IRGenResult},
        generic_codegen::LLVMGenericCodegen,
        typed_codegen::LLVMTypedCodegen,
    },
    compiler::Compiler,
    type_inference::{ConcreteValue, NodeTypeDB, Type},
};

/**
 * Convert AnyTypeEnum to BasicTypeEnum
 */
pub fn any_type_to_basic_type<'ctx>(any_type: AnyTypeEnum<'ctx>) -> Option<BasicTypeEnum<'ctx>> {
    match any_type {
        AnyTypeEnum::ArrayType(ty) => Some(ty.into()),
        AnyTypeEnum::FloatType(ty) => Some(ty.into()),
        AnyTypeEnum::IntType(ty) => Some(ty.into()),
        AnyTypeEnum::PointerType(ty) => Some(ty.into()),
        AnyTypeEnum::StructType(ty) => Some(ty.into()),
        AnyTypeEnum::VectorType(ty) => Some(ty.into()),
        _ => None, // Some types like VoidType can't be converted to BasicTypeEnum
    }
}

/**
 * Helper to get enum of a list's element type
 */
pub fn get_list_element_enum<'ctx>(
    compiler: &mut Compiler<'ctx>,
    llvm_type: AnyTypeEnum<'ctx>,
) -> Option<IntValue<'ctx>> {
    let bool_type = compiler.context.bool_type();
    let str_type = compiler.context.i8_type().ptr_type(AddressSpace::default());
    let list_type = compiler.module.get_struct_type("struct.List").unwrap();
    let range_type = compiler.module.get_struct_type("struct.Range").unwrap();

    let enum_type = compiler.context.i32_type();

    if llvm_type.is_int_type() {
        if llvm_type.into_int_type() == bool_type {
            return Some(enum_type.const_int(1, false));
        }
        return Some(enum_type.const_int(0, false));
    }

    if llvm_type.is_float_type() {
        return Some(enum_type.const_int(2, false));
    }

    if llvm_type.is_pointer_type() {
        let llvm_type_as_ptr = llvm_type.into_pointer_type();
        if llvm_type_as_ptr == str_type {
            return Some(enum_type.const_int(3, false));
        } else if llvm_type_as_ptr == list_type.ptr_type(AddressSpace::default()) {
            println!("element type is list");
            return Some(enum_type.const_int(5, false));
        } else if llvm_type_as_ptr == range_type.ptr_type(AddressSpace::default()) {
            return Some(enum_type.const_int(6, false));
        } else {
            return Some(enum_type.const_int(4, false));
        }
    }

    None
}

/**
 * Helper mapping types to their corresponding LLVM type
 */
pub fn get_llvm_type<'ctx>(compiler: &mut Compiler<'ctx>, typ: &Type) -> Option<AnyTypeEnum<'ctx>> {
    match typ {
        Type::ConcreteType(ConcreteValue::Int) => {
            Some(compiler.context.i64_type().as_any_type_enum())
        }
        Type::ConcreteType(ConcreteValue::Bool) => {
            Some(compiler.context.bool_type().as_any_type_enum())
        }
        Type::ConcreteType(ConcreteValue::Str) => Some(
            compiler
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .as_any_type_enum(),
        ),
        Type::ConcreteType(ConcreteValue::Float) => {
            Some(compiler.context.f64_type().as_any_type_enum())
        }
        Type::List(..) => Some(
            compiler
                .module
                .get_struct_type("struct.List")
                .unwrap()
                .ptr_type(AddressSpace::default())
                .as_any_type_enum(),
        ),
        Type::Range => Some(
            compiler
                .module
                .get_struct_type("struct.Range")
                .unwrap()
                .ptr_type(AddressSpace::default())
                .as_any_type_enum(),
        ),
        _ => None,
    }
}

/**
 * Helper to check if a value is iterable
 */
pub fn is_iterable<'ctx>(compiler: &mut Compiler<'ctx>, value: &AnyValueEnum<'ctx>) -> bool {
    let range_type = compiler.module.get_struct_type("struct.Range").unwrap();
    let list_type = compiler.module.get_struct_type("struct.List").unwrap();
    let iter_type = compiler.module.get_struct_type("struct.Iterator").unwrap();
    let str_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    if !value.get_type().is_pointer_type() {
        return false;
    }

    let val_ptr_type = value.get_type().into_pointer_type();

    return val_ptr_type == range_type.ptr_type(AddressSpace::default())
        || val_ptr_type == iter_type.ptr_type(AddressSpace::default())
        || val_ptr_type == list_type.ptr_type(AddressSpace::default())
        || val_ptr_type == str_type;
}

/**
 * Helper to build generic range call
 */
pub fn build_generic_range_call<'ctx>(
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

    let new_int_fn = compiler.module.get_function("new_int").unwrap();

    let start = if args.len() > 1 {
        args[0]
    } else {
        let zero = compiler.context.i64_type().const_int(0, false);
        compiler
            .builder
            .build_call(
                new_int_fn,
                &[compiler
                    .convert_any_value_to_param_value(zero.as_any_value_enum())
                    .expect("Could not convert int to param value.")],
                "start_obj",
            )
            .expect("Could not call new_int.")
            .as_any_value_enum()
    };
    let stop = if args.len() > 1 { args[1] } else { args[0] };
    let step = if args.len() == 3 {
        args[2]
    } else {
        let one = compiler.context.i64_type().const_int(1, false);
        compiler
            .builder
            .build_call(
                new_int_fn,
                &[compiler
                    .convert_any_value_to_param_value(one.as_any_value_enum())
                    .expect("Could not convert int to param value.")],
                "start_obj",
            )
            .expect("Could not call new_int.")
            .as_any_value_enum()
    };

    let build_range_obj_fn = match compiler.module.get_function("build_range_obj") {
        Some(func) => func,
        None => {
            let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
            let arg_types = Vec::from([
                compiler
                    .convert_any_type_to_param_type(obj_ptr_type.as_any_type_enum())
                    .unwrap(),
                compiler
                    .convert_any_type_to_param_type(obj_ptr_type.as_any_type_enum())
                    .unwrap(),
                compiler
                    .convert_any_type_to_param_type(obj_ptr_type.as_any_type_enum())
                    .unwrap(),
            ]);
            let build_range_obj_fn_type = obj_ptr_type.fn_type(&arg_types, false);
            let _ = compiler
                .module
                .add_function("build_range_obj", build_range_obj_fn_type, None);
            compiler.module.get_function("build_range_obj").unwrap()
        }
    };
    let build_range_obj_call = compiler.builder.build_call(
        build_range_obj_fn,
        &[
            compiler.convert_any_value_to_param_value(start).unwrap(),
            compiler.convert_any_value_to_param_value(stop).unwrap(),
            compiler.convert_any_value_to_param_value(step).unwrap(),
        ],
        "range_obj",
    );

    match build_range_obj_call {
        Ok(ir) => Ok(ir.as_any_value_enum()),
        Err(..) => Err(BackendError {
            message: "Could not build call.",
        }),
    }
}

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
 * Helper to build for loop body for generic codegen
 */
pub fn build_generic_for_loop_body<'ctx>(
    compiler: &mut Compiler<'ctx>,
    iter_ptr: &PointerValue<'ctx>,
    next_func: FunctionValue<'ctx>,
    body: &[Stmt],
    orelse: &[Stmt],
    target: &Expr,
) -> IRGenResult<'ctx> {
    let target_name = if let Some(name) = target.as_name_expr() {
        name.id.as_str()
    } else {
        return Err(BackendError {
            message: "Target in for loop is not a name.",
        });
    };

    let curr_block = compiler.builder.get_insert_block().unwrap();

    let loop_cond_block = compiler
        .context
        .insert_basic_block_after(curr_block, "loop_cond");
    let loop_body_block = compiler
        .context
        .insert_basic_block_after(loop_cond_block, "loop_body");
    let loop_orelse_block = if orelse.len() > 0 {
        compiler
            .context
            .insert_basic_block_after(loop_cond_block, "loop_orelse")
    } else {
        compiler
            .context
            .insert_basic_block_after(loop_body_block, "loop_end")
    };
    let loop_end_block = if orelse.len() > 0 {
        compiler
            .context
            .insert_basic_block_after(loop_body_block, "loop_end")
    } else {
        loop_orelse_block
    };

    {
        compiler.sym_table.enter_scope();
    }

    let _ = compiler.builder.build_unconditional_branch(loop_cond_block);

    // loop_cond
    let _ = compiler.builder.position_at_end(loop_cond_block);
    let iter_ptr_as_param = compiler
        .convert_any_value_to_param_value(iter_ptr.as_any_value_enum())
        .expect("Could not convert iterator to param value.");

    let next_value = compiler
        .builder
        .build_call(next_func, &[iter_ptr_as_param.into()], "next_val")
        .expect("Could not increment iterator.")
        .as_any_value_enum();

    let target_ptr = allocate_variable(compiler, target_name, &next_value.as_any_value_enum())?;
    let _ = compiler.builder.build_store(
        target_ptr.into_pointer_value(),
        next_value.into_pointer_value(),
    );
    compiler
        .sym_table
        .add_variable(target_name, None, Some(target_ptr.as_any_value_enum()));
    let is_null = compiler
        .builder
        .build_is_null(next_value.into_pointer_value(), "is_null")
        .expect("Could not build is_null.");
    let _ = compiler
        .builder
        .build_conditional_branch(is_null, loop_orelse_block, loop_body_block);

    // loop_body
    compiler.builder.position_at_end(loop_body_block);
    for stmt in body {
        stmt.generic_codegen(compiler)?;
    }

    let _ = compiler.builder.build_unconditional_branch(loop_cond_block);

    // loop_orelse
    if orelse.len() > 0 {
        compiler.builder.position_at_end(loop_orelse_block);
        for stmt in orelse {
            stmt.generic_codegen(compiler)?;
        }
        let _ = compiler.builder.build_unconditional_branch(loop_end_block);
    }

    // End loop
    compiler.builder.position_at_end(loop_end_block);

    {
        compiler.sym_table.exit_scope();
    }

    Ok(is_null.as_any_value_enum())
}

/**
 * Helper to build iterator increment
 */
pub fn build_iter_increment<'ctx>(
    compiler: &mut Compiler<'ctx>,
    iter_ptr: PointerValue<'ctx>,
    next_func: FunctionValue<'ctx>,
    target_type: BasicTypeEnum<'ctx>,
) -> IRGenResult<'ctx> {
    let void_ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    let iter_as_void_ptr = compiler
        .builder
        .build_pointer_cast(iter_ptr, void_ptr_type, "iter_as_void_ptr")
        .expect("Could not cast iter pointer.");

    let next_value = compiler
        .builder
        .build_call(next_func, &[iter_as_void_ptr.into()], "target")
        .expect("Failed to call next function")
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    let target_ptr_type = target_type.ptr_type(AddressSpace::default());

    let next_value_as_elt = compiler
        .builder
        .build_pointer_cast(next_value, target_ptr_type, "next_as_elt_ptr")
        .expect("Could not cast target to element type.");

    Ok(next_value_as_elt.as_any_value_enum())
}

/**
 * Helper to build for loop body.
 */
pub fn build_typed_for_loop_body<'ctx>(
    compiler: &mut Compiler<'ctx>,
    type_db: &NodeTypeDB,
    iter_ptr: PointerValue<'ctx>,
    next_func: FunctionValue<'ctx>,
    target: &Expr,
    body: &[Stmt],
    orelse: &[Stmt],
) -> IRGenResult<'ctx> {
    let target_name = if let Some(name) = target.as_name_expr() {
        name.id.as_str()
    } else {
        return Err(BackendError {
            message: "Target in for loop is not a name.",
        });
    };

    let curr_block = compiler.builder.get_insert_block().unwrap();

    let loop_cond_block = compiler
        .context
        .insert_basic_block_after(curr_block, "loop_cond");
    let loop_body_block = compiler
        .context
        .insert_basic_block_after(loop_cond_block, "loop_body");
    let loop_orelse_block = if orelse.len() > 0 {
        compiler
            .context
            .insert_basic_block_after(loop_cond_block, "loop_orelse")
    } else {
        compiler
            .context
            .insert_basic_block_after(loop_body_block, "loop_end")
    };
    let loop_end_block = if orelse.len() > 0 {
        compiler
            .context
            .insert_basic_block_after(loop_body_block, "loop_end")
    } else {
        loop_orelse_block
    };

    {
        compiler.sym_table.enter_scope();
    }

    let _ = compiler.builder.build_unconditional_branch(loop_cond_block);

    // loop_cond
    compiler.builder.position_at_end(loop_cond_block);
    let target_type = if let Some(typ) = type_db.get(&target.range()) {
        let type_as_any_enum = get_llvm_type(compiler, typ).expect("Invalid target type.");
        any_type_to_basic_type(type_as_any_enum).expect("Invalid target type.")
    } else {
        return Err(BackendError {
            message: "Target type is not defined.",
        });
    };
    let target =
        build_iter_increment(compiler, iter_ptr, next_func, target_type)?.into_pointer_value();
    compiler
        .sym_table
        .add_variable(target_name, Some(target.as_any_value_enum()), None);
    let is_null = compiler
        .builder
        .build_is_null(target, "is_null")
        .expect("Could not build is_null.");
    let _ = compiler
        .builder
        .build_conditional_branch(is_null, loop_orelse_block, loop_body_block);

    // loop_body
    compiler.builder.position_at_end(loop_body_block);
    for stmt in body {
        stmt.typed_codegen(compiler, &type_db)?;
    }

    let _ = compiler.builder.build_unconditional_branch(loop_cond_block);

    // loop_orelse
    if orelse.len() > 0 {
        compiler.builder.position_at_end(loop_orelse_block);
        for stmt in orelse {
            stmt.typed_codegen(compiler, &type_db)?;
        }
        let _ = compiler.builder.build_unconditional_branch(loop_end_block);
    }

    // End loop
    compiler.builder.position_at_end(loop_end_block);

    {
        compiler.sym_table.exit_scope();
    }

    Ok(is_null.as_any_value_enum())
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
