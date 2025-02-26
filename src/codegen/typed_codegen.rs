use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, FloatValue, IntValue};
use inkwell::AddressSpace;
use malachite_bigint;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprCompare, ExprConstant, ExprContext,
    ExprIfExp, ExprList, ExprName, ExprUnaryOp, Operator, Stmt, StmtAssign, StmtExpr, StmtFor,
    StmtFunctionDef, StmtIf, StmtWhile, UnaryOp,
};

use std::collections::HashMap;

use crate::astutils::get_iter_type_name;
use crate::compiler::Compiler;
use crate::compiler_utils::builder_utils::{
    allocate_variable, build_range_call, build_typed_for_loop_body, handle_global_assignment,
    is_iterable, store_value,
};
use crate::compiler_utils::get_predicate::{get_float_predicate, get_int_predicate};
use crate::compiler_utils::print_fn::print_fn;
use crate::type_inference::{ConcreteValue, Type, TypeEnv};

use super::error::{BackendError, IRGenResult};
use super::generic_codegen::LLVMGenericCodegen;

pub trait LLVMTypedCodegen {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir>;
}

impl LLVMTypedCodegen for Stmt {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
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
                message: "Stmt type not implemented yet...",
            }),
        }
    }
}

impl LLVMTypedCodegen for StmtFunctionDef {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        // save main entry point
        let main_entry = compiler
            .builder
            .get_insert_block()
            .expect("Builder isn't mapped to a basic block?");

        let func_name = self.name.as_str();
        let func_type;
        match types.get(func_name) {
            Some(scheme) => func_type = scheme.type_name.clone(),
            None => {
                return Err(BackendError {
                    message: "Function {func_name} not typed.",
                })
            }
        }

        {
            let _ = compiler.sym_table.enter_scope();
        }

        // get argument and return types
        let mut arg_types: Vec<Type> = Vec::new();
        let return_type: Type;
        match *func_type {
            Type::FuncType(func) => {
                let initial_input = *func.input.clone();
                // Don't add argument if it has type None (to deal with empty args)
                match initial_input {
                    Type::ConcreteType(ConcreteValue::None) => {}
                    _ => arg_types.push(*func.input.clone()),
                }
                let mut current_output = &*func.output;
                while let Type::FuncType(funcval) = current_output {
                    arg_types.push(*funcval.input.clone());
                    current_output = &*funcval.output;
                }
                return_type = current_output.clone();
            }
            _ => {
                return Err(BackendError {
                    message: "Function {func_name} type is not a FuncType.",
                })
            }
        }

        let llvm_arg_types: Vec<BasicMetadataTypeEnum<'_>> = arg_types
            .into_iter()
            .map(|arg_type| {
                match arg_type {
                    Type::ConcreteType(ConcreteValue::Int) => compiler.context.i64_type().into(),
                    Type::ConcreteType(ConcreteValue::Float) => compiler.context.f64_type().into(),
                    Type::ConcreteType(ConcreteValue::Bool) => compiler.context.bool_type().into(),
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

        // TODO: PLEASE PLEASE PLEASE refactor this omg it's so ugly wtf
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
            // TODO: Extend to handle Any cases in the case of ambiguous return types
            _ => {
                return Err(BackendError {
                    message: "The compiler needs a bit more help to infer this function, please annotate the function with more information!",
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
                let _ = compiler.builder.build_store(v_ptr, v);
                (k.clone(), v_ptr.as_any_value_enum())
            })
            .collect::<HashMap<_, _>>();

        {
            let mut func_args = compiler.func_args.borrow_mut();
            for (arg_name, arg_val) in arg_map {
                compiler
                    .sym_table
                    .add_variable(&arg_name, Some(arg_val), None);
                func_args.push(arg_name);
            }
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
                    message: "Return statement cannot be declared twice in function body.",
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
                Type::ConcreteType(ConcreteValue::Str) => {
                    let _ = compiler
                        .builder
                        .build_return(Some(&ir.into_pointer_value()));
                }
                _ => {
                    return Err(BackendError {
                        message: "Not a valid function return type.",
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

        // // Build generic function body
        let _ = self.generic_codegen(compiler)?;

        Ok(func_def.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for StmtAssign {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        // TODO: Figure out how to deal with subscripts
        let target_name = match &self.targets[0] {
            Expr::Name(exprname) => exprname.id.to_string(),
            _ => {
                return Err(BackendError {
                    message: "Left of an assignment must be a variable",
                })
            }
        };

        let typed_value = self.value.typed_codegen(compiler, types)?;

        let is_function_arg = {
            let func_args = compiler.func_args.borrow();
            func_args.contains(&target_name)
        };

        let generic_value_ptr = if !is_function_arg {
            Some(self.value.generic_codegen(compiler)?)
        } else {
            None
        };

        let target_ptr = if compiler.sym_table.is_global_scope() {
            handle_global_assignment(
                compiler,
                &target_name,
                &Some(typed_value),
                &generic_value_ptr,
            )?;
            let (g_ptr, _) = compiler
                .sym_table
                .resolve_variable(&target_name)
                .expect("Target should be defined.");
            g_ptr.expect("Global pointer should not be none when compiling with types.")
        } else {
            match compiler.sym_table.resolve_variable(&target_name) {
                Some((ptr, _)) => ptr.expect(
                    "Variable is defined in this scope but doesn't have a corresponding pointer...",
                ),
                _ => allocate_variable(compiler, &target_name, &typed_value)?,
            }
        }
        .into_pointer_value();

        store_value(compiler, &target_ptr, &typed_value)?;

        Ok(target_ptr.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for StmtFor {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        let iter = self.iter.typed_codegen(compiler, types)?;
        if !is_iterable(compiler, &iter) {
            return Err(BackendError {
                message: "Invalid iterator type.",
            });
        }

        let iter_type = get_iter_type_name(&self.iter, types);

        if iter_type.is_empty() {
            return Err(BackendError {
                message: "Iterator type not implemented yet.",
            });
        }

        let iter_func_name = format!("{}_iter", iter_type);
        let next_func_name = format!("{}_next", iter_type);

        // TODO: Declare all iter functions in compiler setup, but can dynamically
        // declare next
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
        types: &TypeEnv,
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

        Ok(while_branch.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for StmtIf {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
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
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        match self {
            Expr::BinOp(binop) => binop.typed_codegen(compiler, types),
            Expr::Constant(constant) => constant.typed_codegen(compiler, types),
            Expr::Name(name) => name.typed_codegen(compiler, types),
            Expr::Call(call) => call.typed_codegen(compiler, types),
            Expr::BoolOp(boolop) => boolop.typed_codegen(compiler, types),
            Expr::Compare(comp) => comp.typed_codegen(compiler, types),
            Expr::UnaryOp(unop) => unop.typed_codegen(compiler, types),
            Expr::IfExp(ifexp) => ifexp.typed_codegen(compiler, types),
            Expr::List(list) => list.typed_codegen(compiler, types),
            _ => Err(BackendError {
                message: "Expression not implemented yet...",
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprList {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "List not implemented yet.",
        })
    }
}

impl LLVMTypedCodegen for ExprIfExp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "IfExp not implemented yet...",
        })
    }
}

impl LLVMTypedCodegen for ExprCompare {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
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
        let ops = self.ops.clone();

        let op_and_comp = ops
            .into_iter()
            .zip(comparators.into_iter())
            .collect::<Vec<(_, _)>>();
        let mut conditions = Vec::new();

        if left.is_vector_value() {
            return Err(BackendError {
                message: "Compare not implemented for list yet.",
            });
        }

        let f64_type = compiler.context.f64_type();
        for (op, comp) in op_and_comp {
            if comp.is_float_value() {
                if left.get_type().is_int_type() {
                    left = compiler
                        .builder
                        .build_signed_int_to_float(left.into_int_value(), f64_type, "")
                        .expect("Could not cast signed int to float.")
                        .as_any_value_enum();
                } else {
                    // Types don't match so false straight away
                    conditions.push(
                        compiler
                            .context
                            .bool_type()
                            .const_int(u64::from(false), false),
                    );
                    break;
                }
            }

            let new_comp = if left.is_float_value() && !comp.is_float_value() {
                compiler
                    .builder
                    .build_signed_int_to_float(
                        comp.into_int_value(),
                        compiler.context.f64_type(),
                        "",
                    )
                    .unwrap()
                    .as_any_value_enum()
            } else {
                comp
            };
            let llvm_comp = if left.is_float_value() {
                let float_predicate = get_float_predicate(op);
                compiler.builder.build_float_compare(
                    float_predicate,
                    left.into_float_value(),
                    new_comp.into_float_value(),
                    "",
                )
            } else if left.is_int_value() {
                let int_predicate = get_int_predicate(op);
                compiler.builder.build_int_compare(
                    int_predicate,
                    left.into_int_value(),
                    new_comp.into_int_value(),
                    "",
                )
            } else {
                return Err(BackendError {
                    message: "Compare not implemented for sequences and sets yet.",
                });
            }
            .expect("Could not compile comparator.");
            conditions.push(llvm_comp);
            left = new_comp;
        }

        let mut composite_comp = conditions[0];
        for cond in conditions {
            composite_comp = compiler
                .builder
                .build_and(composite_comp, cond, "")
                .expect("Could not build 'and' for compare.");
        }
        Ok(composite_comp.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for ExprUnaryOp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
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
                // TODO: Extend for sequence, set, and string types...
                if i == false_val {
                    Ok(truth_val.as_any_value_enum())
                } else if i == truth_val {
                    Ok(false_val.as_any_value_enum())
                } else {
                    // i64 type
                    if i == zero {
                        Ok(truth_val.as_any_value_enum())
                    } else {
                        Ok(false_val.as_any_value_enum())
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
                message: "Invalid operand for given unary op.",
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprBoolOp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        let values = self
            .values
            .clone()
            .into_iter()
            .map(|val| val.typed_codegen(compiler, types).unwrap())
            .collect::<Vec<_>>();

        let true_val = compiler
            .context
            .bool_type()
            .const_int(u64::from(true), false);
        let false_val = compiler
            .context
            .bool_type()
            .const_int(u64::from(false), false);
        let res;

        if self.op.is_and() {
            for val in values {
                if val == false_val {
                    return Ok(false_val.as_any_value_enum());
                }
            }
            res = true_val;
        } else {
            // op is "or"
            for val in values {
                if val == true_val {
                    return Ok(true_val.as_any_value_enum());
                }
            }
            res = false_val;
        }

        Ok(res.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for ExprCall {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        let func_name = self
            .func
            .as_name_expr()
            .expect("You can only call functions...?")
            .id
            .as_str();
        let function;
        if func_name.eq("print") {
            function = compiler
                .module
                .get_function("printf")
                .expect("Could not find print function.");
        } else if func_name.eq("range") {
            let args = self
                .args
                .iter()
                .map(|arg| arg.typed_codegen(compiler, types))
                .collect::<Result<Vec<_>, BackendError>>()?;
            return build_range_call(compiler, args);
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
                message: "Incorrect number of arguments provided.",
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
        let fn_arg_types = function.get_type().get_param_types();
        for i in 0..arg_count {
            let expected_type = fn_arg_types[i as usize].as_any_type_enum();
            if expected_type != args[i as usize].get_type() {
                return self.generic_codegen(compiler);
            }
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
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        match self.ctx {
            ExprContext::Load | ExprContext::Store => {
                let name = self.id.as_str();
                let var_ptr = compiler.sym_table.resolve_variable(name);

                match var_ptr {
                    Some((ptr, _)) => {
                        if ptr.is_none() {
                            return Err(
                                BackendError { message: "Variable is defined in this scope but doesn't have a corresponding pointer..." }
                            );
                        }
                        let load = compiler
                            .builder
                            .build_load(ptr.unwrap().into_pointer_value(), name)
                            .expect("Could not load variable.");
                        Ok(load.as_any_value_enum())
                    }
                    _ => Err(BackendError {
                        message: "Variable {name} is not defined.",
                    }),
                }
            }
            ExprContext::Del => Err(BackendError {
                message: "Deleting a variable is not implemented yet.",
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprConstant {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
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
                message: "Constant type not implemented yet...",
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprBinOp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &mut Compiler<'ctx>,
        types: &TypeEnv,
    ) -> IRGenResult<'ir> {
        // TODO: Figure out what to do if I have like a result from a generic func and
        // I try to do some operations on it
        let op = self.op;
        let left = self.left.typed_codegen(compiler, types)?;
        let right = self.right.typed_codegen(compiler, types)?;

        if !left.is_float_value()
            && !left.is_int_value()
            && !left.is_pointer_value()
            && !left.is_vector_value()
        {
            return Err(BackendError {
                message: "Invalid left operand for binary operator",
            });
        }

        if !right.is_float_value()
            && !right.is_int_value()
            && !right.is_pointer_value()
            && !right.is_vector_value()
        {
            return Err(BackendError {
                message: "Invalid right operand for binary operator.",
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
                    // String concat
                    let strconcat_fn = compiler.module.get_function("strconcat").unwrap();
                    let res = compiler
                        .builder
                        .build_call(
                            strconcat_fn,
                            &[
                                left.into_pointer_value().into(),
                                right.into_pointer_value().into(),
                            ],
                            "",
                        )
                        .expect("Could not create string from object.");
                    res.as_any_value_enum()
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
            _ => {
                return Err(BackendError {
                    message: "Unsupported operator",
                })
            }
        };

        Ok(res)
    }
}

// HELPER FUNCTIONS

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
