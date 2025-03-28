use inkwell::{
    types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, PointerType},
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue,
    },
    AddressSpace,
};
use rustpython_ast::{CmpOp, ExprAttribute, ExprSubscript, Ranged, StmtAssign};
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

use super::get_predicate::get_int_predicate;

/**
 * Helper to build print_obj call
 */
pub fn build_print_obj_call<'ctx>(
    compiler: &mut Compiler<'ctx>,
    args: &[AnyValueEnum<'ctx>],
) -> IRGenResult<'ctx> {
    let print_obj_fn = compiler.module.get_function("print_obj").unwrap();
    let llvm_arg_count = compiler
        .context
        .i32_type()
        .const_int(args.len() as u64, false)
        .as_any_value_enum();
    let mut args = Vec::from(args);
    args.insert(0, llvm_arg_count);

    let args: Vec<BasicMetadataValueEnum<'_>> = args
        .into_iter()
        .filter_map(|val| compiler.convert_any_value_to_param_value(val))
        .collect();

    let print_obj_call = compiler.builder.build_call(print_obj_fn, &args, "");

    let print_newline_fn = compiler.module.get_function("print_newline").unwrap();
    let _ = compiler.builder.build_call(print_newline_fn, &[], "");

    match print_obj_call {
        Ok(res) => Ok(res.as_any_value_enum()),
        Err(..) => Err(BackendError {
            message: "Could not call print_obj.",
        }),
    }
}

/**
 * Helper to perform generic compare expression
 */
pub fn build_generic_comp_op<'ctx>(
    compiler: &mut Compiler<'ctx>,
    g_cmpop_name: &str,
    left: &AnyValueEnum<'ctx>,
    comp: &AnyValueEnum<'ctx>,
) -> IRGenResult<'ctx> {
    let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
    let g_cmpop_fn = match compiler.module.get_function(g_cmpop_name) {
        Some(func) => func,
        None => {
            let params = [obj_ptr_type, obj_ptr_type]
                .iter()
                .map(|p| {
                    compiler
                        .convert_any_type_to_param_type(p.as_any_type_enum())
                        .unwrap()
                })
                .collect::<Vec<_>>();

            let g_op_fn_type = compiler.context.bool_type().fn_type(&params, false);
            compiler
                .module
                .add_function(&g_cmpop_name, g_op_fn_type, None)
        }
    };
    let left = create_object(compiler, *left)?;
    let comp = create_object(compiler, *comp)?;

    let llvm_comp = compiler
        .builder
        .build_call(
            g_cmpop_fn,
            &[
                BasicMetadataValueEnum::PointerValue(left.into_pointer_value()),
                BasicMetadataValueEnum::PointerValue(comp.into_pointer_value()),
            ],
            "cmp_result",
        )
        .expect(format!("Could not perform generic {:?}.", g_cmpop_name).as_str())
        .as_any_value_enum();

    Ok(llvm_comp)
}

/**
 * Helper to initialise Object* from a typed value
 */
pub fn create_object<'ctx>(
    compiler: &mut Compiler<'ctx>,
    value: AnyValueEnum<'ctx>,
) -> IRGenResult<'ctx> {
    // Check if given value is already an object
    let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
    if value.is_pointer_value() && value.into_pointer_value().get_type() == obj_ptr_type {
        return Ok(value);
    }

    let list_ptr_type = compiler
        .module
        .get_struct_type("struct.List")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let range_ptr_type = compiler
        .module
        .get_struct_type("struct.Range")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let str_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    let new_object_fn = match value.get_type() {
        AnyTypeEnum::IntType(v) if v == compiler.context.bool_type() => compiler
            .module
            .get_function("new_bool")
            .expect("new_bool isn't defined"),
        AnyTypeEnum::IntType(..) => compiler
            .module
            .get_function("new_int")
            .expect("new_int isn't defined"),
        AnyTypeEnum::FloatType(..) => compiler
            .module
            .get_function("new_float")
            .expect("new_float isn't defined"),
        AnyTypeEnum::PointerType(ptr_type) if ptr_type == list_ptr_type => compiler
            .module
            .get_function("new_list")
            .expect("new_list isn't defined"),
        AnyTypeEnum::PointerType(ptr_type) if ptr_type == range_ptr_type => compiler
            .module
            .get_function("new_range")
            .expect("new_range isn't defined"),
        AnyTypeEnum::PointerType(ptr_type) if ptr_type == str_type => compiler
            .module
            .get_function("new_str")
            .expect("new_str isn't defined"),
        _ => {
            if value.is_pointer_value() {
                println!("here {:?}", value);
                let ptr_val = compiler
                    .builder
                    .build_load(value.into_pointer_value(), "")
                    .expect("Could not load from pointer");
                return create_object(compiler, ptr_val.as_any_value_enum());
            } else {
                return Err(BackendError {
                    message: "Given value cannot be converted to Object*.",
                });
            }
        }
    };

    let new_object = compiler
        .builder
        .build_call(
            new_object_fn,
            &[compiler.convert_any_value_to_param_value(value).unwrap()],
            "boxed_value",
        )
        .expect("Could not convert value to Object*.");

    Ok(new_object.as_any_value_enum())
}

/**
 * Helper to handle function calls that are attributes
 * e.g. x.append(3)
 * Only really handles append calls...
 */
pub fn handle_attr_fn_call<'ctx>(
    compiler: &mut Compiler<'ctx>,
    types: &NodeTypeDB,
    attr: &ExprAttribute,
    args: &[AnyValueEnum<'ctx>],
) -> IRGenResult<'ctx> {
    // only handles append
    if !attr.attr.as_str().eq("append") {
        return Err(BackendError {
            message: "Invalid function call.",
        });
    }

    if args.len() != 1 {
        return Err(BackendError {
            message: "Invalid number of arguments given to append call.",
        });
    }

    let list_ptr_type = compiler
        .module
        .get_struct_type("struct.List")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let void_ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    let value_ptr = if let Some(Type::List(..)) = types.get(&attr.value.range()) {
        let value_codegen = attr.value.typed_codegen(compiler, types)?;
        compiler
            .builder
            .build_pointer_cast(value_codegen.into_pointer_value(), list_ptr_type, "")
            .expect("Could not cast pointer to list type.")
    } else {
        return Err(BackendError {
            message: "Can only call append on lists.",
        });
    };

    let arg = args[0];

    let list_append_fn = compiler
        .module
        .get_function("list_append")
        .expect("list_append is not defined.");

    let arg_ptr = match arg.get_type().size_of() {
        Some(size) => compiler
            .build_gc_malloc_call(size)
            .expect("Could not allocate memory to value."),
        _ => {
            return Err(BackendError {
                message: "Invalid argument for append.",
            })
        }
    }
    .into_pointer_value();
    let arg_ptr = compiler
        .builder
        .build_pointer_cast(
            arg_ptr,
            any_type_to_basic_type(arg.get_type())
                .expect("Invalid argument for append.")
                .ptr_type(AddressSpace::default()),
            "",
        )
        .expect("Could not cast arg pointer.");

    store_value(compiler, &arg_ptr, &arg)?;

    let arg_ptr_as_void_ptr = compiler
        .builder
        .build_pointer_cast(arg_ptr, void_ptr_type, "")
        .expect("Could not cast pointer to void type.")
        .as_any_value_enum();

    let params: Vec<_> = Vec::from([value_ptr.as_any_value_enum(), arg_ptr_as_void_ptr])
        .into_iter()
        .map(|arg| {
            compiler
                .convert_any_value_to_param_value(arg)
                .expect("Invalid param type for subscript assignment.")
        })
        .collect();

    let append_call = compiler
        .builder
        .build_call(list_append_fn, &params, "append");

    match append_call {
        Ok(res) => Ok(res.as_any_value_enum()),
        Err(..) => Err(BackendError {
            message: "Could not append value to list.",
        }),
    }
}

/**
 * Helper to handle subscript assignment
 */
pub fn handle_subscript_assignment<'ctx>(
    compiler: &mut Compiler<'ctx>,
    types: &NodeTypeDB,
    subscript: &ExprSubscript,
    assign_stmt: &StmtAssign,
) -> IRGenResult<'ctx> {
    let list_ptr_type = compiler
        .module
        .get_struct_type("struct.List")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let void_ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    let subscript_value = subscript.value.typed_codegen(compiler, types)?;
    let subscript_value = match types.get(&subscript.value.range()) {
        Some(typ) => {
            let llvm_type = get_llvm_type(compiler, typ)
                .expect("Invalid type of subscript.");
            if !llvm_type.is_pointer_type() || llvm_type.into_pointer_type() != list_ptr_type {
                return Err(BackendError {
                    message: "Subscripted type doesn't support item assignment.",
                });
            }
            compiler
                .builder
                .build_pointer_cast(
                    subscript_value.into_pointer_value(),
                    llvm_type.into_pointer_type(),
                    "",
                )
                .expect("Could not cast pointer.")
        }
        _ => {
            return Err(BackendError {
                message: "Only lists and dictionaries can be subscripted.",
            })
        }
    };

    let slice = subscript.slice.typed_codegen(compiler, types)?;

    if !slice.is_int_value() {
        return Err(BackendError {
            message: "Dictionaries are not implemented yet.",
        });
    }

    let subscript_value_type_string =
        get_llvm_type_name(compiler, &subscript_value.as_any_value_enum());
    if subscript_value_type_string.is_empty() {
        return Err(BackendError {
            message: "Invalid subscript value type.",
        });
    }
    let set_fn_name = format!("{subscript_value_type_string}_set");
    let set_fn_err_msg = format!("{set_fn_name} is not defined.");
    let set_fn = compiler
        .module
        .get_function(&set_fn_name)
        .expect(set_fn_err_msg.as_str());

    let rhs = assign_stmt.value.typed_codegen(compiler, types)?;
    let rhs_ptr = match rhs.get_type().size_of() {
        Some(size) => compiler
            .build_gc_malloc_call(size)
            .expect("Could not allocate memory to value."),
        _ => {
            return Err(BackendError {
                message: "Invalid RHS of assignment.",
            })
        }
    }
    .into_pointer_value();
    let rhs_ptr = compiler
        .builder
        .build_pointer_cast(
            rhs_ptr,
            any_type_to_basic_type(rhs.get_type())
                .expect("Invalid RHS of assignment.")
                .ptr_type(AddressSpace::default()),
            "",
        )
        .expect("Could not cast pointer.");

    store_value(compiler, &rhs_ptr, &rhs)?;

    let rhs_ptr_as_void_ptr = compiler
        .builder
        .build_pointer_cast(rhs_ptr, void_ptr_type, "")
        .expect("Could not cast pointer to void type.")
        .as_any_value_enum();

    let params: Vec<_> = Vec::from([
        subscript_value.as_any_value_enum(),
        slice,
        rhs_ptr_as_void_ptr,
    ])
    .into_iter()
    .map(|arg| {
        compiler
            .convert_any_value_to_param_value(arg)
            .expect("Invalid param type for subscript assignment.")
    })
    .collect();

    let set_call = compiler.builder.build_call(set_fn, &params, "set");

    match set_call {
        Ok(res) => Ok(res.as_any_value_enum()),
        Err(..) => Err(BackendError {
            message: "Could not set subscript value.",
        }),
    }
}

/**
 * Helper to handle string comparisons
 */
pub fn handle_str_compare<'ctx>(
    compiler: &mut Compiler<'ctx>,
    left: PointerValue<'ctx>,
    right: PointerValue<'ctx>,
    op: &CmpOp,
) -> IntValue<'ctx> {
    if op.is_eq() {
        let str_eq_fn = compiler
            .module
            .get_function("str_eq")
            .expect("str_eq is not defined.");
        return compiler
            .builder
            .build_call(str_eq_fn, &[left.into(), right.into()], "")
            .expect("Could not call str_eq.")
            .as_any_value_enum()
            .into_int_value();
    }

    let str_len_fn = compiler
        .module
        .get_function("str_len")
        .expect("str_len is not defined.");
    let left_len = compiler
        .builder
        .build_call(str_len_fn, &[left.into()], "left_len")
        .expect("Could not call str_len.")
        .as_any_value_enum()
        .into_int_value();

    let right_len = compiler
        .builder
        .build_call(str_len_fn, &[right.into()], "right_len")
        .expect("Could not call str_len.")
        .as_any_value_enum()
        .into_int_value();

    compiler
        .builder
        .build_int_compare(get_int_predicate(*op), left_len, right_len, "str_cmp")
        .expect("Failed to generate comparison")
}

/**
 * Helper to handle predefined functions.
 */
pub fn handle_predefined_functions<'ctx>(
    compiler: &mut Compiler<'ctx>,
    args: Vec<AnyValueEnum<'ctx>>,
    func_name: &str,
) -> IRGenResult<'ctx> {
    if func_name.eq("range") {
        let res = build_range_call(compiler, args);
        return res;
    }
    if func_name.eq("len") {
        let res = build_len_call(compiler, args);
        return res;
    }

    Err(BackendError {
        message: "Function name is not predefined.",
    })
}

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
            return Some(enum_type.const_int(5, false));
        } else if llvm_type_as_ptr == range_type.ptr_type(AddressSpace::default()) {
            return Some(enum_type.const_int(6, false));
        } else {
            return Some(enum_type.const_int(4, false));
        }
    }

    None
}

pub fn get_llvm_type_name<'ctx>(
    compiler: &mut Compiler<'ctx>,
    value: &AnyValueEnum<'ctx>,
) -> String {
    let value_as_ptr_type = if value.is_pointer_value() {
        value.into_pointer_value().get_type()
    } else {
        return "".to_string();
    };

    let list_ptr_type = compiler
        .module
        .get_struct_type("struct.List")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let range_ptr_type = compiler
        .module
        .get_struct_type("struct.Range")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let str_ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    if value_as_ptr_type == list_ptr_type {
        return "list".to_string();
    } else if value_as_ptr_type == range_ptr_type {
        return "range".to_string();
    } else if value_as_ptr_type == str_ptr_type {
        return "str".to_string();
    }

    "".to_string()
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
 * Helper to build generic len call
 */
pub fn build_generic_len_call<'ctx>(
    compiler: &mut Compiler<'ctx>,
    args: &[AnyValueEnum<'ctx>],
) -> IRGenResult<'ctx> {
    if args.len() != 1 {
        return Err(BackendError {
            message: "len call takes only one argument.",
        });
    }
    let object_len_fn = compiler
        .module
        .get_function("object_len")
        .expect("object_len is not defined.");
    let param = compiler
        .convert_any_value_to_param_value(args[0])
        .expect("Could not convert value to param.");

    let object_len_call = compiler
        .builder
        .build_call(object_len_fn, &[param], "")
        .expect("Could not call object_len.");

    Ok(object_len_call.as_any_value_enum())
}

/**
 * Helper to create len call.
 */
pub fn build_len_call<'ctx>(
    compiler: &mut Compiler<'ctx>,
    args: Vec<AnyValueEnum<'ctx>>,
) -> IRGenResult<'ctx> {
    if args.len() != 1 {
        return Err(BackendError {
            message: "A single argument must be given to the len call.",
        });
    }

    let arg = args[0];
    if !arg.is_pointer_value() {
        return Err(BackendError {
            message: "Argument to len call must be a list, string, range, or dictionary.",
        });
    }
    let list_ptr_type = compiler
        .module
        .get_struct_type("struct.List")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let range_ptr_type = compiler
        .module
        .get_struct_type("struct.Range")
        .unwrap()
        .ptr_type(AddressSpace::default());
    let str_ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());

    let arg_type = arg.into_pointer_value().get_type();
    let arg_type_string = if arg_type == list_ptr_type {
        "list"
    } else if arg_type == range_ptr_type {
        "range"
    } else if arg_type == str_ptr_type {
        "str"
    } else {
        return Err(BackendError {
            message: "Argument to len call must be a list, string, range, or dictionary.",
        });
    };

    let len_fn_name = format!("{arg_type_string}_len");
    let len_fn_err_msg = format!("{len_fn_name} has not been defined.");
    let len_fn = compiler
        .module
        .get_function(&len_fn_name)
        .expect(&len_fn_err_msg.as_str());
    let arg_as_param = compiler
        .convert_any_value_to_param_value(arg)
        .expect("Could not convert len call argument to param.");

    let len_call_err_msg = format!("Could not call {len_fn_name}");
    let len_call = compiler
        .builder
        .build_call(len_fn, &[arg_as_param], "")
        .expect(len_call_err_msg.as_str());

    Ok(len_call.as_any_value_enum())
}

/**
 * Helper to build generic range call
 */
pub fn build_generic_range_call<'ctx>(
    compiler: &mut Compiler<'ctx>,
    args: &[AnyValueEnum<'ctx>],
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

    // TODO: Add generic version of target ptr by loading the value and boxing it with create_object()
    let target =
        build_iter_increment(compiler, iter_ptr, next_func, target_type)?.into_pointer_value();
    let is_null = compiler
        .builder
        .build_is_null(target, "is_null")
        .expect("Could not build is_null.");
    let _ = compiler
        .builder
        .build_conditional_branch(is_null, loop_orelse_block, loop_body_block);

    // loop_body
    compiler.builder.position_at_end(loop_body_block);
    let target_obj = create_object(compiler, target.as_any_value_enum())?;
    let target_obj_ptr = allocate_variable(compiler, target_name, &target_obj)?;
    let _ = store_value(compiler, &target_obj_ptr.into_pointer_value(), &target_obj)?;

    compiler.sym_table.add_variable(
        target_name,
        Some(target.as_any_value_enum()),
        Some(target_obj_ptr),
    );
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
        AnyTypeEnum::FloatType(..) => {
            global.set_initializer(&compiler.context.f64_type().const_zero())
        }
        AnyTypeEnum::IntType(..) => {
            global.set_initializer(&compiler.context.i64_type().const_zero())
        }
        AnyTypeEnum::PointerType(p) => {
            global.set_initializer(&p.const_null());
        }
        _ => {
            return Err(BackendError {
                message: "Unsupported r-value type.",
            })
        }
    }

    store_value(compiler, &global.as_pointer_value(), value)?;
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
