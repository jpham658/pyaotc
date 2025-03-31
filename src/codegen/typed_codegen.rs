use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, FloatValue, IntValue};
use inkwell::AddressSpace;
use malachite_bigint;
use rustpython_ast::{ExprSubscript, Ranged};
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprCompare, ExprConstant, ExprContext,
    ExprList, ExprName, ExprUnaryOp, Operator, Stmt, StmtAssign, StmtExpr, StmtFor,
    StmtFunctionDef, StmtIf, StmtWhile, UnaryOp,
};

use std::collections::HashMap;

use crate::astutils::get_iter_type_name;
use crate::compiler::Compiler;
use crate::compiler_utils::builder_utils::{
    allocate_variable, any_type_to_basic_type, build_generic_comp_op, build_typed_for_loop_body,
    create_object, get_list_element_enum, get_llvm_type, get_llvm_type_name, handle_attr_fn_call,
    handle_global_assignment, handle_predefined_functions, handle_str_compare,
    handle_subscript_assignment, is_iterable, store_value,
};
use crate::compiler_utils::get_predicate::{get_float_predicate, get_int_predicate};
use crate::compiler_utils::print_fn::print_fn;
use crate::type_inference::{extract_func_types, ConcreteValue, NodeTypeDB, Type};

use super::error::{BackendError, IRGenResult};
use super::generic_codegen::LLVMGenericCodegen;

pub trait LLVMTypedCodegen {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir>;
}

impl LLVMTypedCodegen for Stmt {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.typed_codegen(compiler, types),
            Stmt::Assign(assign) => assign.typed_codegen(compiler, types),
            Stmt::If(ifstmt) => ifstmt.typed_codegen(compiler, types),
            Stmt::While(whilestmt) => whilestmt.typed_codegen(compiler, types),
            Stmt::For(forstmt) => forstmt.typed_codegen(compiler, types),
            Stmt::Return(return_stmt) => match &return_stmt.value {
                None => {
                    let ret_none = compiler
                        .builder
                        .build_return(None)
                        .expect("Could not build void return.");
                    Ok(ret_none.as_any_value_enum())
                }
                Some(expr) => expr.typed_codegen(compiler, types),
            },
            Stmt::FunctionDef(funcdef) => funcdef.typed_codegen(compiler, types),
            _ => Err(BackendError {
                message:
                    "This program uses a statement type that is not supported by the compiler."
                        .to_string(),
            }),
        }
    }
}

impl LLVMTypedCodegen for StmtFunctionDef {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let main_entry = compiler
            .builder
            .get_insert_block()
            .expect("Builder isn't mapped to a basic block?");
        let func_name = self.name.as_str();

        let func_type;
        match types.get(&self.range) {
            Some(typ) => func_type = typ.clone(),
            None => {
                return Err(BackendError {
                    message: format!("Function {func_name} not typed."),
                })
            }
        }

        compiler
            .func_types
            .borrow_mut()
            .insert(func_name.to_string(), func_type.clone());

        {
            let _ = compiler.sym_table.enter_scope();
        }

        // get argument and return types
        let mut func_types = extract_func_types(&func_type);
        let func_type_err_msg = format!(
            "Type for function {} ({:?}) is invalid.",
            func_name, func_type
        );
        let return_type = func_types.pop().expect(&func_type_err_msg);

        let llvm_arg_types: Vec<BasicMetadataTypeEnum<'_>> = func_types
            .into_iter()
            .map(|arg_type| {
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
                match arg_type {
                    Type::ConcreteType(ConcreteValue::Int) => compiler.context.i64_type().into(),
                    Type::ConcreteType(ConcreteValue::Float) => compiler.context.f64_type().into(),
                    Type::ConcreteType(ConcreteValue::Bool) => compiler.context.bool_type().into(),
                    Type::List(..) => list_ptr_type.into(),
                    Type::Range => range_ptr_type.into(),
                    _ => {
                        compiler
                            .context
                            .i8_type()
                            .ptr_type(AddressSpace::default())
                            .into() // TODO: better way to handle this
                    }
                }
            })
            .collect();

        let llvm_return_type = match &return_type {
            Type::ConcreteType(ConcreteValue::Int) => {
                compiler.context.i64_type().fn_type(&llvm_arg_types, false)
            }
            Type::ConcreteType(ConcreteValue::Float) => {
                compiler.context.f64_type().fn_type(&llvm_arg_types, false)
            }
            Type::ConcreteType(ConcreteValue::Bool) => {
                compiler.context.bool_type().fn_type(&llvm_arg_types, false)
            }
            Type::ConcreteType(ConcreteValue::Str) => compiler
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .fn_type(&llvm_arg_types, false),
            Type::ConcreteType(ConcreteValue::None) => {
                compiler.context.void_type().fn_type(&llvm_arg_types, false)
            }
            Type::List(..) => {
                let list_type = compiler.module.get_struct_type("struct.List").unwrap();
                list_type
                    .ptr_type(AddressSpace::default())
                    .fn_type(&llvm_arg_types, false)
            }
            Type::Range => {
                let range_type = compiler.module.get_struct_type("struct.Range").unwrap();
                range_type
                    .ptr_type(AddressSpace::default())
                    .fn_type(&llvm_arg_types, false)
            }
            Type::Any => {
                return Err(BackendError {
                    message: "Functions with more than one return type are not supported."
                        .to_string(),
                });
            }
            _ => {
                return Err(BackendError {
                    message: "The compiler needs a bit more help to infer this function, please annotate the function with more information!".to_string(),
                });
            }
        };

        let func_def = compiler
            .module
            .add_function(func_name, llvm_return_type, None);

        let func_entry = compiler.context.append_basic_block(func_def, "entry");
        compiler.builder.position_at_end(func_entry);

        // make map for arguments, and pointers to their arguments
        let args = self
            .args
            .args
            .clone()
            .into_iter()
            .map(|arg| arg.def.arg.as_str().to_string())
            .collect::<Vec<_>>();
        let arg_map = args
            .into_iter()
            .zip(func_def.get_param_iter())
            .collect::<HashMap<_, _>>()
            .into_iter()
            .map(|(k, v)| {
                let v_ptr = compiler
                    .builder
                    .build_alloca(v.get_type(), "")
                    .expect("Could not allocate memory for function argument.");
                let _ = store_value(compiler, &v_ptr, &v.as_any_value_enum());
                (k.clone(), v_ptr.as_any_value_enum())
            })
            .collect::<HashMap<_, _>>();

        let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
        let mut arg_names = Vec::new();
        for (arg_name, arg_val) in arg_map {
            let generic_func_arg = compiler
                .builder
                .build_alloca(obj_ptr_type, "")
                .expect("Could not allocate stack memory.")
                .as_any_value_enum();

            let arg = compiler
                .builder
                .build_load(arg_val.into_pointer_value(), "")
                .expect("Could not load function argument.");
            let arg_as_obj = create_object(compiler, arg.as_any_value_enum())
                .expect("Could not create object for function argument.");

            let _ = store_value(
                compiler,
                &generic_func_arg.into_pointer_value(),
                &arg_as_obj,
            );

            compiler
                .sym_table
                .add_variable(&arg_name, Some(arg_val), Some(generic_func_arg));

            arg_names.push(arg_name);
        }
        {
            let mut func_args = compiler.func_args.borrow_mut();
            func_args.extend(arg_names);
        }

        let mut ret_stmt_declared = false;

        // build function body
        for stmt in &self.body {
            let ir = stmt.typed_codegen(compiler, types)?;
            if !stmt.is_return_stmt() {
                continue;
            }
            if ret_stmt_declared {
                return Err(BackendError {
                    message: "Return statement defined more than once in this scope.".to_string(),
                });
            }
            ret_stmt_declared = true;
            match &return_type {
                Type::ConcreteType(ConcreteValue::Int)
                | Type::ConcreteType(ConcreteValue::Bool) => {
                    let _ = compiler.builder.build_return(Some(&ir.into_int_value()));
                }
                Type::ConcreteType(ConcreteValue::Float) => {
                    let _ = compiler.builder.build_return(Some(&ir.into_float_value()));
                }
                Type::ConcreteType(ConcreteValue::None) => {
                    continue;
                }
                Type::ConcreteType(ConcreteValue::Str) | Type::List(..) | Type::Range => {
                    let _ = compiler
                        .builder
                        .build_return(Some(&ir.into_pointer_value()));
                }
                _ => {
                    return Err(BackendError {
                        message: format!("Invalid return type {:?}.", return_type),
                    })
                }
            };
        }

        if !ret_stmt_declared {
            let _ = compiler.builder.build_return(None);
        }

        // reset back to normal!
        compiler.builder.position_at_end(main_entry);
        {
            let _ = compiler.sym_table.exit_scope();
            compiler.func_args.borrow_mut().clear();
        }

        // Build generic function body
        let _ = self.generic_codegen(compiler)?;

        Ok(func_def.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for StmtAssign {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        if let Some(subscript) = self.targets[0].as_subscript_expr() {
            let typed_res = handle_subscript_assignment(compiler, types, subscript, self);
            if let Err(e) = self.generic_codegen(compiler) {
                return Err(e);
            }
            return typed_res;
        }

        let target_name = match &self.targets[0] {
            Expr::Name(exprname) => exprname.id.to_string(),
            _ => {
                return Err(BackendError {
                    message: "Left of an assignment must be a variable.".to_string(),
                })
            }
        };

        let typed_value = self.value.typed_codegen(compiler, types)?;

        let is_function_arg = {
            let func_args = compiler.func_args.borrow();
            func_args.contains(&target_name)
        };

        let generic_value_ptr = if !is_function_arg {
            match self.value.generic_codegen(compiler) {
                Ok(res) => Some(res),
                Err(..) => Some(create_object(compiler, typed_value)?),
            }
        } else {
            None
        };

        if compiler.sym_table.is_global_scope() {
            handle_global_assignment(
                compiler,
                &target_name,
                &Some(typed_value),
                &generic_value_ptr,
            )?;
        } else {
            match compiler.sym_table.resolve_variable(&target_name) {
                Some((existing_ptr, existing_generic_ptr)) => {
                    let existing_ptr = existing_ptr
                        .expect("Variable is defined but has no pointer.")
                        .into_pointer_value();
                    store_value(compiler, &existing_ptr, &typed_value)?;
                    if let Some(generic) = &generic_value_ptr {
                        if let Some(generic_ptr) = existing_generic_ptr {
                            store_value(compiler, &generic_ptr.into_pointer_value(), &generic)?;
                        } else {
                            let ptr_to_generic =
                                allocate_variable(compiler, &target_name, generic)?;
                            store_value(compiler, &ptr_to_generic.into_pointer_value(), &generic)?;
                        }
                    }
                }
                _ => {
                    let typed_ptr = allocate_variable(compiler, &target_name, &typed_value)?;
                    let generic_ptr = if let Some(generic) = generic_value_ptr {
                        Some(allocate_variable(compiler, &target_name, &generic)?)
                    } else {
                        None
                    };
                    compiler
                        .sym_table
                        .add_variable(&target_name, Some(typed_ptr), generic_ptr);
                }
            }
        }

        let (target_ptr, generic_target_ptr) = compiler
            .sym_table
            .resolve_variable(&target_name)
            .expect("Target should be defined.");

        store_value(
            compiler,
            &target_ptr
                .expect("Pointer should be defined for typed variables in typed codegen phase.")
                .into_pointer_value(),
            &typed_value,
        )?;

        if let Some(ptr) = generic_target_ptr {
            if let Some(val) = generic_value_ptr {
                store_value(compiler, &ptr.into_pointer_value(), &val)?;
            }
        }

        Ok(target_ptr.unwrap().as_any_value_enum())
    }
}

impl LLVMTypedCodegen for StmtFor {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let iter = self.iter.typed_codegen(compiler, types)?;
        if !is_iterable(compiler, &iter) {
            return Err(BackendError {
                message: format!("Invalid iterator type. {:?}", iter),
            });
        }

        let iter_type = get_iter_type_name(&self.iter, types);

        if iter_type.is_empty() {
            return Err(BackendError {
                message: format!("Iterator type {:?} not implemented yet.", iter),
            });
        }

        let iter_func_name = format!("{}_iter", iter_type);
        let next_func_name = format!("{}_next", iter_type);

        let iter_func = compiler.module.get_function(&iter_func_name).unwrap();

        let next_func = match compiler.module.get_function(&next_func_name) {
            Some(func) => func,
            None => {
                let void_ptr = compiler.context.i8_type().ptr_type(AddressSpace::default());
                let next_fn_type = void_ptr.fn_type(&[void_ptr.into()], false);
                let _ = compiler
                    .module
                    .add_function(&next_func_name, next_fn_type, None);
                compiler.module.get_function(&next_func_name).unwrap()
            }
        };

        let iter_as_param = compiler
            .convert_any_value_to_param_value(iter)
            .expect("Iterable could not be converted to a parameter.");

        let iter_ptr = compiler
            .builder
            .build_call(iter_func, &[iter_as_param.into()], "iter")
            .expect("Failed to call iterator function")
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        build_typed_for_loop_body(
            compiler,
            types,
            iter_ptr,
            next_func,
            &self.target,
            &self.body,
            &self.orelse,
        )
    }
}

impl LLVMTypedCodegen for StmtWhile {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let curr_block = compiler.builder.get_insert_block().unwrap();
        let while_test = compiler
            .context
            .insert_basic_block_after(curr_block, "while_test");
        let while_body = compiler
            .context
            .insert_basic_block_after(while_test, "while_body");
        let while_end = compiler
            .context
            .insert_basic_block_after(while_body, "while_end");
        let while_or_else = if self.orelse.is_empty() {
            None
        } else {
            Some(
                compiler
                    .context
                    .insert_basic_block_after(while_body, "while_or_else"),
            )
        };
        let _ = compiler.builder.build_unconditional_branch(while_test);

        // while_test
        let _ = compiler.builder.position_at_end(while_test);
        let test = self.test.typed_codegen(compiler, types)?;
        let test_as_bool = convert_test_to_bool(&test, compiler);
        let while_branch = match while_or_else {
            Some(or_else_block) => compiler
                .builder
                .build_conditional_branch(test_as_bool, while_body, or_else_block)
                .expect("Could not build while test."),
            _ => compiler
                .builder
                .build_conditional_branch(test_as_bool, while_body, while_end)
                .expect("Could not build while test."),
        };

        let _ = compiler.sym_table.enter_scope();

        // while_body
        let _ = compiler.builder.position_at_end(while_body);
        let mut ret_stmt_in_while_body = false;
        for stmt in &self.body {
            match stmt.typed_codegen(compiler, types) {
                Err(e) => return Err(e),
                Ok(ir) => {
                    if stmt.is_return_stmt() {
                        ret_stmt_in_while_body = true;
                        match ir {
                            AnyValueEnum::IntValue(i) => {
                                let _ = compiler.builder.build_return(Some(&i));
                            }
                            AnyValueEnum::FloatValue(f) => {
                                let _ = compiler.builder.build_return(Some(&f));
                            }
                            AnyValueEnum::PointerValue(p) => {
                                let _ = compiler.builder.build_return(Some(&p));
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        if !ret_stmt_in_while_body {
            let _ = compiler.builder.build_unconditional_branch(while_test);
        }

        // while_or_else
        if let Some(or_else_block) = while_or_else {
            let _ = compiler.builder.position_at_end(or_else_block);
            let mut ret_stmt_in_or_else = false;
            for stmt in &self.orelse {
                match stmt.typed_codegen(compiler, types) {
                    Err(e) => return Err(e),
                    Ok(ir) => {
                        if stmt.is_return_stmt() {
                            ret_stmt_in_or_else = true;
                            match ir {
                                AnyValueEnum::IntValue(i) => {
                                    let _ = compiler.builder.build_return(Some(&i));
                                }
                                AnyValueEnum::FloatValue(f) => {
                                    let _ = compiler.builder.build_return(Some(&f));
                                }
                                AnyValueEnum::PointerValue(p) => {
                                    let _ = compiler.builder.build_return(Some(&p));
                                }
                                _ => {
                                    let _ = compiler.builder.build_return(None);
                                }
                            }
                        }
                    }
                }
            }
            if !ret_stmt_in_or_else {
                let _ = compiler.builder.build_unconditional_branch(while_end);
            }
        }

        // while_end
        let _ = compiler.builder.position_at_end(while_end);

        let _ = compiler.sym_table.exit_scope();

        Ok(while_branch.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for StmtIf {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let test = self.test.typed_codegen(compiler, types)?;
        let curr_block = compiler.builder.get_insert_block().unwrap();
        let iftrue = compiler
            .context
            .insert_basic_block_after(curr_block, "iftrue");
        let iffalse = if self.orelse.is_empty() {
            None
        } else {
            Some(compiler.context.insert_basic_block_after(iftrue, "iffalse"))
        };
        let ifend = compiler.context.insert_basic_block_after(iftrue, "ifend");
        let if_cond_block = if let Some(iffalse_block) = iffalse {
            iffalse_block
        } else {
            ifend
        };

        let test_as_bool = convert_test_to_bool(&test, compiler);

        let branch = compiler
            .builder
            .build_conditional_branch(test_as_bool, iftrue, if_cond_block)
            .expect("Could not build if statement branch.");

        // iftrue
        let _ = compiler.builder.position_at_end(iftrue);
        let mut ret_stmt_in_true = false;
        for stmt in &self.body {
            match stmt.typed_codegen(compiler, types) {
                Err(e) => return Err(e),
                Ok(ir) => {
                    if stmt.is_return_stmt() {
                        ret_stmt_in_true = true;
                        match ir {
                            AnyValueEnum::IntValue(i) => {
                                let _ = compiler.builder.build_return(Some(&i));
                            }
                            AnyValueEnum::FloatValue(f) => {
                                let _ = compiler.builder.build_return(Some(&f));
                            }
                            AnyValueEnum::PointerValue(p) => {
                                let _ = compiler.builder.build_return(Some(&p));
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        if !ret_stmt_in_true {
            let _ = compiler.builder.build_unconditional_branch(ifend);
        }

        // iffalse
        if let Some(iffalse_block) = iffalse {
            let _ = compiler.builder.position_at_end(iffalse_block);
            let mut ret_stmt_in_iffalse = false;
            for stmt in &self.orelse {
                match stmt.typed_codegen(compiler, types) {
                    Err(e) => return Err(e),
                    Ok(ir) => {
                        if stmt.is_return_stmt() {
                            ret_stmt_in_iffalse = true;
                            match ir {
                                AnyValueEnum::IntValue(i) => {
                                    let _ = compiler.builder.build_return(Some(&i));
                                }
                                AnyValueEnum::FloatValue(f) => {
                                    let _ = compiler.builder.build_return(Some(&f));
                                }
                                AnyValueEnum::PointerValue(p) => {
                                    let _ = compiler.builder.build_return(Some(&p));
                                }
                                _ => {
                                    let _ = compiler.builder.build_return(None);
                                }
                            }
                        }
                    }
                }
            }
            if !ret_stmt_in_iffalse {
                let _ = compiler.builder.build_unconditional_branch(ifend);
            }
        }

        let _ = compiler.builder.position_at_end(ifend);

        Ok(branch.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for Expr {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        match self {
            Expr::BinOp(binop) => binop.typed_codegen(compiler, types),
            Expr::Constant(constant) => constant.typed_codegen(compiler, types),
            Expr::Name(name) => name.typed_codegen(compiler, types),
            Expr::Call(call) => call.typed_codegen(compiler, types),
            Expr::BoolOp(boolop) => boolop.typed_codegen(compiler, types),
            Expr::Compare(comp) => comp.typed_codegen(compiler, types),
            Expr::UnaryOp(unop) => unop.typed_codegen(compiler, types),
            Expr::List(list) => list.typed_codegen(compiler, types),
            Expr::Subscript(subscript) => subscript.typed_codegen(compiler, types),
            _ => Err(BackendError {
                message: "Expression not implemented yet...".to_string(),
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprSubscript {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let value = self.value.typed_codegen(compiler, types)?;
        let slice = self.slice.typed_codegen(compiler, types)?;

        if !slice.is_int_value() {
            return Err(BackendError {
                message: "Non-integer slices are not implemented yet.".to_string(),
            });
        }

        let value_type_string = get_llvm_type_name(compiler, &value);

        if value_type_string.is_empty() {
            return Err(BackendError {
                message: "Invalid value type.".to_string(),
            });
        }

        let index_fn_name = format!("{value_type_string}_index");
        let index_fn_err_msg = format!("{index_fn_name} has not been defined.");
        let index_fn = compiler
            .module
            .get_function(&index_fn_name)
            .expect(&index_fn_err_msg);

        let call_index_fn_err_msg = format!("Could not call {index_fn_name}.");
        let subscript_ptr = compiler
            .builder
            .build_call(
                index_fn,
                &[
                    compiler
                        .convert_any_value_to_param_value(value)
                        .expect("Could not convert value to param."),
                    compiler
                        .convert_any_value_to_param_value(slice)
                        .expect("Could not convert slice to param."),
                ],
                "subscript_ptr",
            )
            .expect(&call_index_fn_err_msg)
            .as_any_value_enum();

        if value_type_string == "range" {
            let subscript = compiler
                .builder
                .build_load(subscript_ptr.into_pointer_value(), "range_subscript")
                .expect("Could not load subscript pointer.");

            return Ok(subscript.as_any_value_enum());
        }

        if let Some(element_type) = types.get(&self.range()) {
            println!("{:?}", element_type);
            let llvm_element_type =
                get_llvm_type(compiler, &element_type).expect("Invalid element type.");
            let llvm_element_type =
                any_type_to_basic_type(llvm_element_type).expect("Invald element type.");

            let subscript_ptr_casted = compiler
                .builder
                .build_pointer_cast(
                    subscript_ptr.into_pointer_value(),
                    llvm_element_type.ptr_type(AddressSpace::default()),
                    "subscript_ptr_casted",
                )
                .expect("Could not cast subscript pointer.");

            let subscript = compiler
                .builder
                .build_load(subscript_ptr_casted, "subscript")
                .expect("Could not load casted subscript pointer.");

            return Ok(subscript.as_any_value_enum());
        }

        Err(BackendError {
            message: "Indexing values for anything other than a string, list, or range is not yet implemented.".to_string(),
        })
    }
}

impl LLVMTypedCodegen for ExprList {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let elt_typ = match types.get(&self.range) {
            Some(Type::List(elt_typ)) => elt_typ,
            _ => {
                return Err(BackendError {
                    message: "Type of list is not declared properly.".to_string(),
                })
            }
        };

        let llvm_elt_type = get_llvm_type(compiler, &elt_typ).expect("Invalid list element type.");
        let llvm_elt_type_size = match llvm_elt_type.size_of() {
            Some(size) => size,
            None => {
                return Err(BackendError {
                    message: "List element type is invalid...".to_string(),
                })
            }
        }
        .as_any_value_enum();
        let llvm_elt_enum_type = match get_list_element_enum(compiler, llvm_elt_type) {
            Some(type_enum) => type_enum,
            None => {
                return Err(BackendError {
                    message: "List element type is not supported.".to_string(),
                })
            }
        }
        .as_any_value_enum();

        let llvm_elts: Vec<_> = self
            .elts
            .clone()
            .into_iter()
            .map(|elt| elt.typed_codegen(compiler, types).unwrap())
            .collect();

        let create_list_fn = compiler
            .module
            .get_function("create_list")
            .expect("create_list is not defined.");

        let list_append_fn = compiler
            .module
            .get_function("list_append")
            .expect("list_append is not defined.");

        // call create_list
        let list = compiler
            .builder
            .build_call(
                create_list_fn,
                &[
                    compiler
                        .convert_any_value_to_param_value(llvm_elt_type_size)
                        .unwrap(),
                    compiler
                        .convert_any_value_to_param_value(llvm_elt_enum_type)
                        .unwrap(),
                ],
                "list",
            )
            .expect("Could not create list.")
            .as_any_value_enum();
        let list_as_param = match compiler.convert_any_value_to_param_value(list) {
            Some(param) => param,
            None => {
                return Err(BackendError {
                    message: "Could not convert list to param.".to_string(),
                })
            }
        };

        // call append
        for elt in llvm_elts {
            let llvm_elt_ptr = compiler
                .build_gc_malloc_call(llvm_elt_type_size.into_int_value())?
                .into_pointer_value();

            let typed_elt_ptr = compiler
                .builder
                .build_pointer_cast(
                    llvm_elt_ptr,
                    any_type_to_basic_type(llvm_elt_type)
                        .unwrap()
                        .ptr_type(AddressSpace::default()),
                    "typed_elt_ptr",
                )
                .unwrap();

            let _ = store_value(compiler, &typed_elt_ptr, &elt)?;

            let void_ptr = compiler
                .builder
                .build_pointer_cast(
                    typed_elt_ptr,
                    compiler.context.i8_type().ptr_type(AddressSpace::default()),
                    "void_ptr",
                )
                .unwrap();

            let elt_ptr_as_param =
                match compiler.convert_any_value_to_param_value(void_ptr.as_any_value_enum()) {
                    Some(param) => param,
                    None => {
                        return Err(BackendError {
                            message: "Could not convert list element pointer to param.".to_string(),
                        })
                    }
                };

            if let Err(..) =
                compiler
                    .builder
                    .build_call(list_append_fn, &[list_as_param, elt_ptr_as_param], "")
            {
                return Err(BackendError {
                    message: "Could not append element to list.".to_string(),
                });
            }
        }

        Ok(list)
    }
}

impl LLVMTypedCodegen for ExprCompare {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let mut left = self.left.typed_codegen(compiler, types)?;
        let comparators = self
            .comparators
            .clone()
            .into_iter()
            .map(|comp| {
                comp.typed_codegen(compiler, types)
                    .expect("Could not compile comparator.")
            })
            .collect::<Vec<_>>();

        let ops = &self.ops;
        let op_and_comp = ops
            .into_iter()
            .zip(comparators.into_iter())
            .collect::<Vec<(_, _)>>();

        let mut conditions = Vec::new();
        // TODO: Add list, range, dict support
        let f64_type = compiler.context.f64_type();
        let bool_type = compiler.context.bool_type();
        let str_type = compiler.context.i8_type().ptr_type(AddressSpace::default());
        let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());

        for (op, comp) in op_and_comp {
            // Check if left or comp is generic, if it is then delegate to generic comp
            if (left.is_pointer_value() && left.into_pointer_value().get_type() == obj_ptr_type)
                || (comp.is_pointer_value() && comp.into_pointer_value().get_type() == obj_ptr_type)
            {
                let g_cmpop_name = format!("{:?}", op);
                let generic_comp = build_generic_comp_op(compiler, &g_cmpop_name, &left, &comp)?;
                conditions.push(generic_comp.into_int_value());
                continue;
            }

            let (new_left, new_comp, type_mismatch) = match (left.get_type(), comp.get_type()) {
                (lt, rt) if lt == rt => (left, comp, false),
                (lt, AnyTypeEnum::FloatType(_)) if lt.is_int_type() => (
                    compiler
                        .builder
                        .build_signed_int_to_float(left.into_int_value(), f64_type, "")
                        .expect("Could not cast int to float.")
                        .as_any_value_enum(),
                    comp,
                    false,
                ),
                (AnyTypeEnum::FloatType(_), rt) if rt.is_int_type() => (
                    left,
                    compiler
                        .builder
                        .build_signed_int_to_float(comp.into_int_value(), f64_type, "")
                        .expect("Could not cast int to float.")
                        .as_any_value_enum(),
                    false,
                ),
                _ => (left, comp, true),
            };

            if type_mismatch {
                conditions.push(bool_type.const_int(0, false));
                break;
            }

            let llvm_comp = if new_left.is_float_value() {
                compiler
                    .builder
                    .build_float_compare(
                        get_float_predicate(*op),
                        new_left.into_float_value(),
                        new_comp.into_float_value(),
                        "",
                    )
                    .expect("Failed to generate comparison")
            } else if new_left.is_int_value() {
                compiler
                    .builder
                    .build_int_compare(
                        get_int_predicate(*op),
                        new_left.into_int_value(),
                        new_comp.into_int_value(),
                        "",
                    )
                    .expect("Failed to generate comparison")
            } else if new_left.get_type().is_pointer_type()
                && new_left.get_type().into_pointer_type() == str_type
            {
                handle_str_compare(
                    compiler,
                    new_left.into_pointer_value(),
                    new_comp.into_pointer_value(),
                    op,
                )
            } else {
                return Err(BackendError {
                    message: "Comparison not implemented for sequences and sets yet.".to_string(),
                });
            };
            conditions.push(llvm_comp);
            left = comp;
        }

        let composite_comp = conditions
            .into_iter()
            .reduce(|acc, cond| {
                compiler
                    .builder
                    .build_and(acc, cond, "")
                    .expect("Could not build 'and' condition.")
            })
            .unwrap_or(bool_type.const_int(1, false));

        Ok(composite_comp.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for ExprUnaryOp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let bool_type = compiler.context.bool_type();
        let true_as_u64 = u64::from(true);
        let false_as_u64 = u64::from(false);
        let truth_val = bool_type.const_int(true_as_u64, false);
        let false_val = bool_type.const_int(false_as_u64, false);

        let i64_type = compiler.context.i64_type();
        let zero = i64_type.const_zero();

        let operand = self.operand.typed_codegen(compiler, types)?;

        match (self.op, operand) {
            (UnaryOp::Not, AnyValueEnum::IntValue(i)) => {
                if i.get_type() == compiler.context.bool_type() {
                    match compiler.builder.build_xor(truth_val, i, "not") {
                        Ok(res) => Ok(res.as_any_value_enum()),
                        Err(..) => Err(BackendError {
                            message: "Could not perform not operation.".to_string(),
                        }),
                    }
                } else {
                    // i64 type
                    match compiler.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        zero,
                        i,
                        "not",
                    ) {
                        Ok(res) => Ok(res.as_any_value_enum()),
                        Err(..) => Err(BackendError {
                            message: "Could not perform not operation.".to_string(),
                        }),
                    }
                }
            }
            (UnaryOp::Not, AnyValueEnum::FloatValue(f)) => {
                let f64_type = compiler.context.f64_type();
                if f == f64_type.const_zero() {
                    Ok(truth_val.as_any_value_enum())
                } else {
                    Ok(false_val.as_any_value_enum())
                }
            }
            (UnaryOp::Not, AnyValueEnum::PointerValue(ptr)) => {
                // TODO: Handle Object ptr
                let str_is_truthy_fn = compiler
                    .module
                    .get_function("str_is_truthy")
                    .expect("str_is_truthy is not declared.");
                let str_is_truthy = compiler
                    .builder
                    .build_call(str_is_truthy_fn, &[ptr.into()], "")
                    .expect("Could not call str_is_truthy.")
                    .as_any_value_enum()
                    .into_int_value();
                let not_str = compiler
                    .builder
                    .build_not(str_is_truthy, "")
                    .expect("Could not build not.");
                Ok(not_str.as_any_value_enum())
            }
            (UnaryOp::USub, AnyValueEnum::IntValue(i)) => {
                if i == false_val {
                    Ok(zero.as_any_value_enum())
                } else if i == truth_val {
                    let one = i64_type.const_int(1, false);
                    let minus = compiler
                        .builder
                        .build_int_nsw_neg(one, "")
                        .expect("Could not build negation.");
                    Ok(minus.as_any_value_enum())
                } else {
                    let minus = compiler
                        .builder
                        .build_int_nsw_neg(operand.into_int_value(), "")
                        .expect("Could not build negation.");
                    Ok(minus.as_any_value_enum())
                }
            }
            (UnaryOp::USub, AnyValueEnum::FloatValue(..)) => {
                let minus = compiler
                    .builder
                    .build_float_neg(operand.into_float_value(), "")
                    .expect("Could not build negation.");
                Ok(minus.as_any_value_enum())
            }
            (UnaryOp::UAdd, AnyValueEnum::IntValue(i)) => Ok(i.as_any_value_enum()),
            (UnaryOp::UAdd, AnyValueEnum::FloatValue(f)) => Ok(f.as_any_value_enum()),
            _ => Err(BackendError {
                message: "Invalid operand for given unary op.".to_string(),
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprBoolOp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let values = self
            .values
            .clone()
            .into_iter()
            .map(|val| val.typed_codegen(compiler, types).unwrap())
            .collect::<Vec<_>>();

        if values.len() < 2 {
            return Err(BackendError {
                message: "BoolOp must have at least 2 operands.".to_string(),
            });
        }

        let res = if self.op.is_and() {
            values
                .clone()
                .into_iter()
                .fold(values[0].into_int_value(), |acc, val| {
                    compiler
                        .builder
                        .build_and(acc, val.into_int_value(), "")
                        .expect("Could not build and")
                })
        } else {
            values
                .clone()
                .into_iter()
                .fold(values[0].into_int_value(), |acc, val| {
                    compiler
                        .builder
                        .build_or(acc, val.into_int_value(), "")
                        .expect("Could not build and")
                })
        };

        Ok(res.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for ExprCall {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        if let Some(attr) = self.func.as_attribute_expr() {
            let args = self
                .args
                .iter()
                .map(|arg| arg.typed_codegen(compiler, types))
                .collect::<Result<Vec<_>, BackendError>>()?;
            let typed_attr_call = handle_attr_fn_call(compiler, types, attr, &args);
            if let Err(e) = self.generic_codegen(compiler) {
                return Err(e);
            }
            return typed_attr_call;
        }

        let func_name = self
            .func
            .as_name_expr()
            .expect("You can only call functions...?")
            .id
            .as_str();

        if func_name.eq("range") || func_name.eq("len") {
            let args = self
                .args
                .iter()
                .map(|arg| arg.typed_codegen(compiler, types))
                .collect::<Result<Vec<_>, BackendError>>()?;
            return handle_predefined_functions(compiler, args, func_name);
        }

        let function;
        if func_name.eq("print") {
            function = compiler
                .module
                .get_function("printf")
                .expect("Could not find print function.");
        } else {
            // TODO: Add check here to check for other builtin function names
            function = compiler
                .module
                .get_function(func_name)
                .expect("Could not find function.");
        }

        // validate function args
        let arg_count = function.count_params();

        if !func_name.eq("print") && arg_count != self.args.len() as u32 {
            return Err(BackendError {
                message: format!("Incorrect number of arguments provided to function {func_name}."),
            });
        }

        // codegen args if we have any
        let args = self
            .args
            .iter()
            .map(|arg| arg.typed_codegen(compiler, types))
            .collect::<Result<Vec<_>, BackendError>>()?;

        if func_name.eq("print") {
            return print_fn(compiler, &args);
        }

        // check types of args
        let fn_arg_types = match compiler.func_types.borrow().get(func_name) {
            None => {
                return Err(BackendError {
                    message: format!("Function {func_name} is not defined."),
                })
            }
            Some(typ) => {
                if let Type::FuncType(..) = typ {
                    let mut fn_types = extract_func_types(typ);
                    fn_types.pop();
                    fn_types
                } else {
                    return Err(BackendError {
                        message: format!("Function call {func_name} mapped to incorrect type."),
                    });
                }
            }
        };
        let call_arg_types: Vec<Type> = self
            .args
            .clone()
            .into_iter()
            .map(|arg| {
                let arg_type = types
                    .get(&arg.range())
                    .expect("Call argument not mapped to type...");
                if let Type::Scheme(scheme) = arg_type {
                    *scheme.type_name.clone()
                } else {
                    arg_type.clone()
                }
            })
            .collect();

        if !call_arg_types.eq(&fn_arg_types) {
            return self.generic_codegen(compiler);
        }

        let args: Vec<BasicMetadataValueEnum<'_>> = args
            .into_iter()
            .filter_map(|val| compiler.convert_any_value_to_param_value(val))
            .collect();

        let call = compiler
            .builder
            .build_call(function, &args, "tmpcall")
            .expect("Could not call function.");

        Ok(call.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for ExprName {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        match self.ctx {
            ExprContext::Load | ExprContext::Store => {
                let name = self.id.as_str();

                let var_ptr = compiler.sym_table.resolve_variable(name);

                match var_ptr {
                    Some((typed_ptr, _)) => {
                        if typed_ptr.is_none() {
                            return Err(
                                BackendError { message: "Variable is defined in this scope but doesn't have a corresponding pointer...".to_string() }
                            );
                        }
                        let load = compiler
                            .builder
                            .build_load(typed_ptr.unwrap().into_pointer_value(), name)
                            .expect("Could not load variable.");
                        Ok(load.as_any_value_enum())
                    }
                    _ => Err(BackendError {
                        message: format!("Variable {name} is not defined."),
                    }),
                }
            }
            ExprContext::Del => Err(BackendError {
                message: "Deleting a variable is not implemented yet.".to_string(),
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprConstant {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        match &self.value {
            Constant::Float(num) => {
                let f64_type = compiler.context.f64_type();
                Ok(f64_type.const_float(*num).as_any_value_enum())
            }
            Constant::Int(num) => {
                let i64_type = compiler.context.i64_type();
                let (sign, digits) = num.to_u64_digits();
                // For now, let's just support numbers up to i64
                if digits.len() > 0 {
                    let int_val = digits[0] as u64;
                    let is_minus = match sign {
                        malachite_bigint::Sign::Minus => true,
                        _ => false,
                    };
                    Ok(i64_type.const_int(int_val, is_minus).as_any_value_enum())
                } else {
                    Ok(i64_type.const_zero().as_any_value_enum())
                }
            }
            Constant::Bool(bool) => {
                let bool_type = compiler.context.bool_type();
                let bool_val = u64::from(*bool);
                Ok(bool_type.const_int(bool_val, false).as_any_value_enum())
            }
            Constant::Str(str) => {
                let str_ptr = compiler
                    .builder
                    .build_global_string_ptr(str, "")
                    .expect("Could not create global string ptr for {str}.");
                Ok(str_ptr.as_any_value_enum())
            }
            Constant::None => {
                let ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());
                let ptr = compiler
                    .builder
                    .build_alloca(ptr_type, "")
                    .expect("Could not allocate space for None.");
                let _ = compiler.builder.build_store(ptr, ptr_type.const_null());
                Ok(ptr.as_any_value_enum())
            }
            _ => Err(BackendError {
                message: format!("Constant type {:?} not implemented yet...", self.value),
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprBinOp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &NodeTypeDB,
    ) -> IRGenResult<'ir> {
        let op = self.op;
        let left = self.left.typed_codegen(compiler, types)?;
        let right = self.right.typed_codegen(compiler, types)?;
        let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());

        if left.get_type() == obj_ptr_type.as_any_type_enum()
            || right.get_type() == obj_ptr_type.as_any_type_enum()
        {
            return self.generic_codegen(compiler);
        }

        if !left.is_float_value() && !left.is_int_value() && !left.is_pointer_value() {
            return Err(BackendError {
                message: format!("Invalid left operand {:?} for binary operator", left),
            });
        }

        if !right.is_float_value() && !right.is_int_value() && !right.is_pointer_value() {
            return Err(BackendError {
                message: format!("Invalid right operand {:?} for binary operator.", right),
            });
        }

        let res = match op {
            Operator::Add => {
                if left.is_int_value() && right.is_int_value() {
                    compiler
                        .builder
                        .build_int_add(left.into_int_value(), right.into_int_value(), &"add")
                        .expect("Could not perform int addition")
                        .as_any_value_enum()
                } else if left.is_pointer_value() && right.is_pointer_value() {
                    let list_type = compiler
                        .module
                        .get_struct_type("struct.List")
                        .unwrap()
                        .ptr_type(AddressSpace::default());
                    let str_type = compiler.context.i8_type().ptr_type(AddressSpace::default());
                    let left_ptr_type = left.into_pointer_value().get_type();
                    let right_ptr_type = right.into_pointer_value().get_type();

                    let res = if left_ptr_type == list_type && right_ptr_type == list_type {
                        let list_add_fn = compiler.module.get_function("list_add").unwrap();
                        let res = compiler
                            .builder
                            .build_call(
                                list_add_fn,
                                &[
                                    left.into_pointer_value().into(),
                                    right.into_pointer_value().into(),
                                ],
                                "",
                            )
                            .expect("Could not add lists.")
                            .as_any_value_enum();
                        Some(res)
                    } else if left_ptr_type == str_type && right_ptr_type == str_type {
                        let str_concat_fn = compiler.module.get_function("str_concat").unwrap();
                        let res = compiler
                            .builder
                            .build_call(
                                str_concat_fn,
                                &[
                                    left.into_pointer_value().into(),
                                    right.into_pointer_value().into(),
                                ],
                                "",
                            )
                            .expect("Pointers are not strings.")
                            .as_any_value_enum();
                        Some(res)
                    } else {
                        None
                    };

                    match res {
                        Some(res) => res,
                        _ => {
                            return Err(BackendError {
                                message: "Invalid operand types for addition.".to_string(),
                            });
                        }
                    }
                } else {
                    let float_map = get_left_and_right_as_floats(compiler, left, right);
                    let lhs = float_map
                        .get(&left)
                        .and_then(|opt| *opt)
                        .expect("Left operand could not be converted.");
                    let rhs = float_map
                        .get(&right)
                        .and_then(|opt| *opt)
                        .expect("Right operand could not be converted.");

                    compiler
                        .builder
                        .build_float_add(lhs, rhs, &"fadd")
                        .expect("Could not perform float addition")
                        .as_any_value_enum()
                }
            }
            Operator::Sub => {
                if left.is_int_value() && right.is_int_value() {
                    compiler
                        .builder
                        .build_int_sub(left.into_int_value(), right.into_int_value(), &"sub")
                        .expect("Could not perform int subtraction")
                        .as_any_value_enum()
                } else {
                    let float_map = get_left_and_right_as_floats(compiler, left, right);
                    let lhs = float_map
                        .get(&left)
                        .and_then(|opt| *opt)
                        .expect("Left operand could not be converted.");
                    let rhs = float_map
                        .get(&right)
                        .and_then(|opt| *opt)
                        .expect("Right operand could not be converted.");
                    compiler
                        .builder
                        .build_float_sub(lhs, rhs, &"fsub")
                        .expect("Could not perform float subtraction")
                        .as_any_value_enum()
                }
            }
            Operator::Mult => {
                if left.is_int_value() && right.is_int_value() {
                    compiler
                        .builder
                        .build_int_mul(left.into_int_value(), right.into_int_value(), &"mul")
                        .expect("Could not perform int multiplication")
                        .as_any_value_enum()
                } else if left.is_pointer_value() && right.is_int_value() {
                    let str_type = compiler.context.i8_type().ptr_type(AddressSpace::default());
                    let left_as_ptr = left.into_pointer_value();

                    let res = if left_as_ptr.get_type() == str_type {
                        let str_mult_fn = compiler.module.get_function("str_mult").unwrap();
                        let res = compiler
                            .builder
                            .build_call(
                                str_mult_fn,
                                &[left_as_ptr.into(), right.into_int_value().into()],
                                "",
                            )
                            .expect("Could not multiply string.")
                            .as_any_value_enum();
                        Some(res)
                    } else {
                        None
                    };

                    match res {
                        Some(res) => res,
                        _ => {
                            return Err(BackendError {
                                message: "Invalid operand types for multiplication.".to_string(),
                            });
                        }
                    }
                } else {
                    let float_map = get_left_and_right_as_floats(compiler, left, right);
                    let lhs = float_map
                        .get(&left)
                        .and_then(|opt| *opt)
                        .expect("Left operand could not be converted.");
                    let rhs = float_map
                        .get(&right)
                        .and_then(|opt| *opt)
                        .expect("Right operand could not be converted.");
                    compiler
                        .builder
                        .build_float_mul(lhs, rhs, &"fmul")
                        .expect("Could not perform float multiplication")
                        .as_any_value_enum()
                }
            }
            Operator::Div => {
                let lhs;
                let rhs;
                let f64_type = compiler.context.f64_type();
                if let AnyValueEnum::IntValue(left_val) = left {
                    lhs = compiler
                        .builder
                        .build_signed_int_to_float(left_val, f64_type, "")
                        .unwrap();
                } else {
                    lhs = left.into_float_value();
                }
                if let AnyValueEnum::IntValue(right_val) = right {
                    rhs = compiler
                        .builder
                        .build_signed_int_to_float(right_val, f64_type, "")
                        .unwrap();
                } else {
                    rhs = right.into_float_value();
                }

                compiler
                    .builder
                    .build_float_div(lhs, rhs, &"fdiv")
                    .expect("Could not perform float division")
                    .as_any_value_enum()
            }
            Operator::Mod => {
                if left.is_int_value() && right.is_int_value() {
                    compiler
                        .builder
                        .build_int_signed_rem(left.into_int_value(), right.into_int_value(), &"mod")
                        .expect("Could not perform int modulo")
                        .as_any_value_enum()
                } else {
                    let float_map = get_left_and_right_as_floats(compiler, left, right);
                    let lhs = float_map
                        .get(&left)
                        .and_then(|opt| *opt)
                        .expect("Left operand could not be converted.");
                    let rhs = float_map
                        .get(&right)
                        .and_then(|opt| *opt)
                        .expect("Right operand could not be converted.");
                    compiler
                        .builder
                        .build_float_rem(lhs, rhs, &"fmod")
                        .expect("Could not perform float modulo")
                        .as_any_value_enum()
                }
            }
            Operator::FloorDiv => {
                if left.is_int_value() && right.is_int_value() {
                    let div = compiler
                        .builder
                        .build_int_signed_div(left.into_int_value(), right.into_int_value(), "div")
                        .expect("Could not perform int division");

                    let mod_val = compiler
                        .builder
                        .build_int_signed_rem(left.into_int_value(), right.into_int_value(), "mod")
                        .expect("Could not perform int modulo");

                    let zero = compiler.context.i64_type().const_int(0, false);
                    let cond = compiler
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, mod_val, zero, "is_zero")
                        .expect("Could not build int compare.");
                    let floored_val = compiler
                        .builder
                        .build_int_sub(div, zero, "floored_div")
                        .expect("Could not floor value.");

                    let result = compiler
                        .builder
                        .build_select(cond, div, floored_val, "floored_result")
                        .expect("Could not select floored result");

                    result.as_any_value_enum()
                } else {
                    let float_map = get_left_and_right_as_floats(compiler, left, right);
                    let lhs = float_map
                        .get(&left)
                        .and_then(|opt| *opt)
                        .expect("Left operand could not be converted.");
                    let rhs = float_map
                        .get(&right)
                        .and_then(|opt| *opt)
                        .expect("Right operand could not be converted.");

                    let division_result = compiler
                        .builder
                        .build_float_div(lhs, rhs, "fdiv")
                        .expect("Could not perform float division");

                    let floor_fn = match compiler.module.get_function("llvm.floor.f64") {
                        Some(func) => func,
                        None => {
                            let f64_type = compiler.context.f64_type();
                            let floor_fn_type = f64_type.fn_type(&[f64_type.into()], false);
                            compiler
                                .module
                                .add_function("llvm.floor.f64", floor_fn_type, None)
                        }
                    };

                    let floored_result = compiler
                        .builder
                        .build_call(floor_fn, &[division_result.into()], "floored_fdiv")
                        .expect("Could not floor float division")
                        .as_any_value_enum();

                    floored_result
                }
            }
            _ => {
                return Err(BackendError {
                    message: format!("Unsupported operator {:?}", self.op),
                })
            }
        };

        Ok(res)
    }
}

// HELPER FUNCTIONS

// TODO: Extend for lists + ranges
fn convert_test_to_bool<'ctx>(
    test: &AnyValueEnum<'ctx>,
    compiler: &mut Compiler<'ctx>,
) -> IntValue<'ctx> {
    match test.get_type() {
        AnyTypeEnum::IntType(i) => {
            let bool_type = compiler.context.bool_type();
            if i == bool_type {
                test.into_int_value()
            } else {
                let i64_type = compiler.context.i64_type();
                let zero = i64_type.const_zero();
                compiler
                    .builder
                    .build_int_compare(inkwell::IntPredicate::NE, test.into_int_value(), zero, "")
                    .expect("Could not build truthy value for int.")
            }
        }
        AnyTypeEnum::FloatType(..) => {
            let f64_type = compiler.context.f64_type();
            let zero = f64_type.const_zero();
            compiler
                .builder
                .build_float_compare(
                    inkwell::FloatPredicate::ONE,
                    test.into_float_value(),
                    zero,
                    "",
                )
                .expect("Could not build truthy value for float.")
        }
        AnyTypeEnum::PointerType(..) => {
            let ptr = test.into_pointer_value();
            if ptr.is_null() {
                compiler.context.bool_type().const_zero()
            } else {
                if ptr.get_type() == compiler.any_type.ptr_type(AddressSpace::default()) {
                    let obj_as_truthy_fn = compiler.module.get_function("obj_as_truthy").unwrap();
                    compiler
                        .builder
                        .build_call(obj_as_truthy_fn, &[ptr.into()], "")
                        .expect("Could not call obj_as_truthy")
                        .as_any_value_enum()
                        .into_int_value()
                } else {
                    let str_is_truthy_fn = compiler.module.get_function("str_is_truthy").unwrap();
                    compiler
                        .builder
                        .build_call(str_is_truthy_fn, &[ptr.into()], "")
                        .expect("Could not call str_is_truthy")
                        .as_any_value_enum()
                        .into_int_value()
                }
            }
        }
        _ => compiler.context.bool_type().const_zero(), // others would need to be some sort runtime check...?
    }
}

fn get_left_and_right_as_floats<'ctx>(
    compiler: &mut Compiler<'ctx>,
    left: AnyValueEnum<'ctx>,
    right: AnyValueEnum<'ctx>,
) -> HashMap<AnyValueEnum<'ctx>, Option<FloatValue<'ctx>>> {
    let mut float_map = std::collections::HashMap::new();
    let lhs;
    let rhs;
    let f64_type = compiler.context.f64_type();
    match (left, right) {
        (AnyValueEnum::IntValue(left_int), _) => {
            let lhs_as_float: FloatValue<'ctx> = compiler
                .builder
                .build_signed_int_to_float(left_int, f64_type, "")
                .unwrap()
                .into();
            lhs = Some(lhs_as_float);
            rhs = Some(right.into_float_value());
        }
        (_, AnyValueEnum::IntValue(right_int)) => {
            lhs = Some(left.into_float_value());
            let rhs_as_float: FloatValue<'ctx> = compiler
                .builder
                .build_signed_int_to_float(right_int, f64_type, "")
                .unwrap()
                .into();
            rhs = Some(rhs_as_float);
        }
        (AnyValueEnum::FloatValue(..), AnyValueEnum::FloatValue(..)) => {
            lhs = Some(left.into_float_value());
            rhs = Some(right.into_float_value());
        }
        _ => {
            lhs = None;
            rhs = None;
        }
    }
    float_map.insert(left, lhs);
    float_map.insert(right, rhs);
    float_map
}
