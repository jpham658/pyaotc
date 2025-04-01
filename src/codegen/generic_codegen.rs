use std::collections::HashMap;

use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;
use malachite_bigint;
use rustpython_ast::ExprSubscript;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprCompare, ExprConstant, ExprContext,
    ExprList, ExprName, ExprUnaryOp, Stmt, StmtAssign, StmtExpr, StmtFor, StmtFunctionDef, StmtIf,
    StmtWhile,
};

use crate::astutils::GetReturnStmts;
use crate::compiler::Compiler;
use crate::compiler_utils::builder_utils::{
    allocate_variable, build_generic_comp_op, build_generic_for_loop_body, build_generic_len_call,
    build_generic_range_call, build_print_obj_call, create_object, get_list_element_enum,
    handle_global_assignment, store_value,
};
use crate::compiler_utils::to_any_type::ToAnyType;

use super::error::{BackendError, IRGenResult};

pub trait LLVMGenericCodegen {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir>;
}

impl LLVMGenericCodegen for ExprBinOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let left = self.left.generic_codegen(compiler)?;
        let right = self.right.generic_codegen(compiler)?;
        let g_op_fn_name = format!("{:?}", self.op);
        let g_op_fn = match compiler.module.get_function(&g_op_fn_name) {
            Some(func) => func,
            None => {
                let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
                let params: Vec<_> = [obj_ptr_type, obj_ptr_type]
                    .into_iter()
                    .map(|p| {
                        compiler
                            .convert_any_type_to_param_type(p.as_any_type_enum())
                            .unwrap()
                    })
                    .collect();
                let g_op_fn_type = obj_ptr_type.fn_type(&params, false);
                compiler
                    .module
                    .add_function(&g_op_fn_name, g_op_fn_type, None)
            }
        };
        let g_op_call = compiler
            .builder
            .build_call(
                g_op_fn,
                &[
                    BasicMetadataValueEnum::PointerValue(left.into_pointer_value()),
                    BasicMetadataValueEnum::PointerValue(right.into_pointer_value()),
                ],
                "",
            )
            .expect(format!("Could not perform generic {:?}.", self.op).as_str());
        Ok(g_op_call.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for Stmt {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.generic_codegen(compiler),
            Stmt::Assign(assign) => assign.generic_codegen(compiler),
            Stmt::If(ifstmt) => ifstmt.generic_codegen(compiler),
            Stmt::While(whilestmt) => whilestmt.generic_codegen(compiler),
            Stmt::For(forstmt) => forstmt.generic_codegen(compiler),
            Stmt::Return(return_stmt) => match &return_stmt.value {
                None => {
                    let ret_none = compiler
                        .builder
                        .build_return(None)
                        .expect("Could not build void return.");
                    Ok(ret_none.as_any_value_enum())
                }
                Some(expr) => {
                    expr.generic_codegen(compiler) // refactor to do the return here...
                }
            },
            Stmt::FunctionDef(funcdef) => funcdef.generic_codegen(compiler),
            _ => Err(BackendError {
                message:
                    "This program uses a statement type that is not supported by the compiler."
                        .to_string(),
            }),
        }
    }
}

impl LLVMGenericCodegen for StmtFunctionDef {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let main_entry = compiler
            .builder
            .get_insert_block()
            .expect("Builder isn't mapped to a basic block?");

        {
            compiler.sym_table.enter_scope();
        }

        let func_name = format!("{}_g", self.name.as_str());

        let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
        let void_type = compiler.context.void_type();
        let bool_type = compiler.context.bool_type();

        let any_args: Vec<BasicMetadataTypeEnum<'_>> = self
            .args
            .args
            .clone()
            .into_iter()
            .map(|_a| obj_ptr_type.into())
            .collect();

        // Declare function with Any return type
        let return_stmts = self.clone().get_return_stmts(); // TODO: Remove this
        let func_type = if return_stmts.is_empty() {
            void_type.fn_type(&any_args, false)
        } else {
            let stmt_ret = &return_stmts[0];
            match &stmt_ret.value {
                None => void_type.fn_type(&any_args, false),
                Some(expr) => match **expr {
                    Expr::Compare(..) | Expr::BoolOp(..) => bool_type.fn_type(&any_args, false),
                    _ => obj_ptr_type.fn_type(&any_args, false),
                },
            }
        };

        let func_def = compiler
            .module
            .add_function(func_name.as_str(), func_type, None);
        let func_entry = compiler.context.append_basic_block(func_def, "entry");
        compiler.builder.position_at_end(func_entry);

        // make map for argument values
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
                    .expect("Could not allocate memory for generic function argument.");
                let _ = compiler.builder.build_store(v_ptr, v);
                (k.clone(), v_ptr.as_any_value_enum())
            })
            .collect::<HashMap<_, _>>();

        {
            let mut func_args = compiler.func_args.borrow_mut();
            for (arg_name, arg_val) in arg_map {
                compiler
                    .sym_table
                    .add_variable(&arg_name, None, Some(arg_val));
                func_args.push(arg_name);
            }
        }

        let mut ret_stmt_declared = false;

        // build function body
        for stmt in &self.body {
            let ir = stmt.generic_codegen(compiler)?;
            if !stmt.is_return_stmt() {
                continue;
            }
            if ret_stmt_declared {
                return Err(BackendError {
                    message: "Return statement defined more than once in this scope.".to_string(),
                });
            }
            ret_stmt_declared = true;
            match ir.get_type() {
                AnyTypeEnum::IntType(..) => {
                    let _ = compiler.builder.build_return(Some(&ir.into_int_value()));
                }
                AnyTypeEnum::VoidType(..) => continue,
                _ => {
                    let _ = compiler
                        .builder
                        .build_return(Some(&ir.into_pointer_value()));
                }
            }
        }

        if !ret_stmt_declared {
            let _ = compiler.builder.build_return(None);
        }

        compiler.builder.position_at_end(main_entry);
        {
            compiler.func_args.borrow_mut().clear();
            compiler.sym_table.exit_scope();
        }

        Ok(func_def.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for StmtAssign {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        if let Some(subscript) = self.targets[0].as_subscript_expr() {
            let subscript_value = subscript.value.generic_codegen(compiler)?;
            let slice = subscript.slice.generic_codegen(compiler)?;
            let value = self.value.generic_codegen(compiler)?;
            let object_set_fn = compiler
                .module
                .get_function("object_set")
                .expect("object_set is not defined.");
            let params: Vec<_> = Vec::from([subscript_value, slice, value])
                .into_iter()
                .map(|arg| {
                    compiler
                        .convert_any_value_to_param_value(arg)
                        .expect("Could not convert value to param.")
                })
                .collect();
            let object_set_call = compiler
                .builder
                .build_call(object_set_fn, &params, "")
                .expect("Could not call object_set.");
            return Ok(object_set_call.as_any_value_enum());
        }

        let target_name = match &self.targets[0] {
            Expr::Name(exprname) => exprname.id.as_str(),
            _ => {
                return Err(BackendError {
                    message: "Left of an assignment must be a variable.".to_string(),
                })
            }
        };

        let value = self.value.generic_codegen(compiler)?;

        let target_ptr = if compiler.sym_table.is_global_scope() {
            handle_global_assignment(compiler, &target_name, &None, &Some(value))?;
            let (_, g_obj_ptr) = compiler
                .sym_table
                .resolve_variable(&target_name)
                .expect("Target should be defined.");
            g_obj_ptr.expect("Global obj pointer should not be none when compiling without types.")
        } else {
            match compiler.sym_table.resolve_variable(&target_name) {
                Some((_, ptr)) => ptr.expect(
                    "Variable is defined in this scope but doesn't have a corresponding pointer...",
                ),
                _ => allocate_variable(compiler, &target_name, &value)?,
            }
        }
        .into_pointer_value();

        store_value(compiler, &target_ptr, &value)?;
        compiler
            .sym_table
            .add_variable(&target_name, None, Some(target_ptr.as_any_value_enum()));

        Ok(target_ptr.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for StmtFor {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let iter_obj = self.iter.generic_codegen(compiler)?;
        let iter_type = iter_obj.get_type();
        let bool_type = compiler.context.bool_type();

        if iter_type == bool_type.as_any_type_enum() {
            return Err(BackendError {
                message: "Iterator cannot be a boolean.".to_string(),
            });
        }

        let obj_next_fn = match compiler.module.get_function("object_next") {
            Some(func) => func,
            None => {
                let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
                let obj_next_fn_type = obj_ptr_type.fn_type(&[obj_ptr_type.into()], false);
                let _ = compiler
                    .module
                    .add_function("object_next", obj_next_fn_type, None);
                compiler.module.get_function("object_next").unwrap()
            }
        };

        let object_into_iter_fn = match compiler.module.get_function("object_into_iterator") {
            Some(func) => func,
            None => {
                let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
                let obj_next_fn_type = obj_ptr_type.fn_type(&[obj_ptr_type.into()], false);
                let _ =
                    compiler
                        .module
                        .add_function("object_into_iterator", obj_next_fn_type, None);
                compiler
                    .module
                    .get_function("object_into_iterator")
                    .unwrap()
            }
        };

        let iter = compiler
            .builder
            .build_call(
                object_into_iter_fn,
                &[compiler.convert_any_value_to_param_value(iter_obj).unwrap()],
                "obj_as_iter",
            )
            .expect("Could not call object_into_iterator.")
            .as_any_value_enum();

        build_generic_for_loop_body(
            compiler,
            &iter.into_pointer_value(),
            obj_next_fn,
            &self.body,
            &self.orelse,
            &self.target,
        )
    }
}

impl LLVMGenericCodegen for StmtWhile {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
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
        let test = self.test.generic_codegen(compiler)?;
        let test_as_bool = if test.is_int_value() {
            test.into_int_value()
        } else {
            let is_truthy_fn = compiler.module.get_function("obj_as_truthy").unwrap();
            compiler
                .builder
                .build_call(is_truthy_fn, &[test.into_pointer_value().into()], "")
                .expect("Could not call obj_as_truthy.")
                .as_any_value_enum()
                .into_int_value()
        };
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
            match stmt.generic_codegen(compiler) {
                Err(e) => return Err(e),
                Ok(ir) => {
                    if stmt.is_return_stmt() {
                        ret_stmt_in_while_body = true;
                        match ir {
                            AnyValueEnum::IntValue(i) => {
                                let _ = compiler.builder.build_return(Some(&i));
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
                match stmt.generic_codegen(compiler) {
                    Err(e) => return Err(e),
                    Ok(ir) => {
                        if stmt.is_return_stmt() {
                            ret_stmt_in_or_else = true;
                            match ir {
                                AnyValueEnum::IntValue(i) => {
                                    let _ = compiler.builder.build_return(Some(&i));
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

impl LLVMGenericCodegen for StmtIf {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let test = self.test.generic_codegen(compiler)?;
        let main = compiler.builder.get_insert_block().unwrap();
        let iftrue = compiler.context.insert_basic_block_after(main, "iftrue");
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

        let is_truthy_fn = compiler.module.get_function("object_as_truthy").unwrap();
        let test_as_bool = if test.is_int_value() {
            test.into_int_value()
        } else {
            compiler
                .builder
                .build_call(is_truthy_fn, &[test.into_pointer_value().into()], "")
                .expect("Could not call object_as_truthy.")
                .as_any_value_enum()
                .into_int_value()
        };

        let branch = compiler
            .builder
            .build_conditional_branch(test_as_bool, iftrue, if_cond_block)
            .expect("Could not build if statement branch.");

        // iftrue
        let _ = compiler.builder.position_at_end(iftrue);
        let mut ret_stmt_in_iftrue = false;
        for stmt in &self.body {
            match stmt.generic_codegen(compiler) {
                Err(e) => return Err(e),
                Ok(ir) => {
                    if stmt.is_return_stmt() {
                        ret_stmt_in_iftrue = true;
                        match ir {
                            AnyValueEnum::IntValue(i) => {
                                let _ = compiler.builder.build_return(Some(&i));
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

        if !ret_stmt_in_iftrue {
            let _ = compiler.builder.build_unconditional_branch(ifend);
        }

        // iffalse
        if let Some(iffalse_block) = iffalse {
            let _ = compiler.builder.position_at_end(iffalse_block);
            let mut ret_stmt_in_iffalse = false;
            for stmt in &self.orelse {
                match stmt.generic_codegen(compiler) {
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

impl LLVMGenericCodegen for Expr {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Expr::BinOp(binop) => binop.generic_codegen(compiler),
            Expr::Constant(constant) => constant.generic_codegen(compiler),
            Expr::Name(name) => name.generic_codegen(compiler),
            Expr::Call(call) => call.generic_codegen(compiler),
            Expr::BoolOp(boolop) => boolop.generic_codegen(compiler),
            Expr::Compare(cmp) => cmp.generic_codegen(compiler),
            Expr::UnaryOp(unop) => unop.generic_codegen(compiler),
            Expr::List(list) => list.generic_codegen(compiler),
            Expr::Subscript(subscript) => subscript.generic_codegen(compiler),
            _ => Err(BackendError {
                message: "Expression type not implemented yet...".to_string(),
            }),
        }
    }
}

impl LLVMGenericCodegen for ExprSubscript {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let value = self.value.generic_codegen(compiler)?;
        let slice = self.slice.generic_codegen(compiler)?;
        let object_index_fn = compiler
            .module
            .get_function("object_index")
            .expect("object_index is not defined.");
        let params: Vec<_> = Vec::from([value, slice])
            .into_iter()
            .map(|arg| {
                compiler
                    .convert_any_value_to_param_value(arg)
                    .expect("Could not convert value to param.")
            })
            .collect();
        let object_index_call = compiler
            .builder
            .build_call(object_index_fn, &params, "")
            .expect("Could not call object_index.");

        Ok(object_index_call.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprList {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let llvm_elt_type = compiler.object_type.ptr_type(AddressSpace::default());
        let llvm_elt_type_size = llvm_elt_type.size_of().as_any_value_enum();

        let llvm_elt_enum_type =
            match get_list_element_enum(compiler, llvm_elt_type.as_any_type_enum()) {
                Some(type_enum) => type_enum,
                None => {
                    return Err(BackendError {
                        message: format!("List element type {:?} is not supported.", llvm_elt_type),
                    })
                }
            }
            .as_any_value_enum();

        let llvm_elts: Vec<_> = self
            .elts
            .clone()
            .into_iter()
            .map(|elt| elt.generic_codegen(compiler).unwrap())
            .collect();

        let create_list_fn = compiler
            .module
            .get_function("create_list")
            .expect("create_list is not defined.");

        let list_append_fn = compiler
            .module
            .get_function("list_append")
            .expect("list_append is not defined.");

        let new_list_fn = compiler
            .module
            .get_function("new_list")
            .expect("new_list is not defined.");

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
                    llvm_elt_type.ptr_type(AddressSpace::default()),
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

        let object_list = compiler
            .builder
            .build_call(
                new_list_fn,
                &[compiler.convert_any_value_to_param_value(list).unwrap()],
                "",
            )
            .expect("Could not call new_list.");

        Ok(object_list.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprCompare {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let mut left = self.left.generic_codegen(compiler)?;
        let comparators = self
            .comparators
            .iter()
            .map(|comp| comp.generic_codegen(compiler))
            .collect::<Result<Vec<_>, _>>()?;

        let ops = &self.ops;
        let mut conditions = Vec::new();

        for (op, comp) in ops.iter().zip(comparators.iter()) {
            let g_cmpop_fn_name = format!("{:?}", op);
            let llvm_comp = build_generic_comp_op(compiler, &g_cmpop_fn_name, &left, comp)?;
            conditions.push(llvm_comp.into_int_value());

            left = llvm_comp;
        }

        let composite_comp = conditions
            .into_iter()
            .reduce(|acc, cond| {
                compiler
                    .builder
                    .build_and(acc, cond, "")
                    .expect("Could not build 'and' condition.")
            })
            .unwrap_or(compiler.context.bool_type().const_int(1, false));

        Ok(composite_comp.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprUnaryOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let g_uop_name = format!("{:?}", self.op);
        let g_uop_fn = match compiler.module.get_function(&g_uop_name) {
            Some(func) => func,
            None => {
                let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
                let param = compiler
                    .convert_any_type_to_param_type(obj_ptr_type.as_any_type_enum())
                    .unwrap();
                let g_op_fn_type = if self.op.is_not() {
                    compiler.context.bool_type().fn_type(&[param], false)
                } else {
                    obj_ptr_type.fn_type(&[param], false)
                };
                compiler
                    .module
                    .add_function(&g_uop_name, g_op_fn_type, None)
            }
        };

        let operand_obj = self.operand.generic_codegen(compiler)?.into_pointer_value();
        let g_uop_res = compiler
            .builder
            .build_call(g_uop_fn, &[operand_obj.into()], "")
            .expect(format!("Failed to call {}", g_uop_name).as_str());
        Ok(g_uop_res.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprBoolOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let op_name = format!("{:?}", self.op);

        let bool_op_fn = match compiler.module.get_function(&op_name) {
            Some(func) => func,
            None => {
                let bool_op_fn_type = compiler
                    .context
                    .bool_type()
                    .fn_type(&[compiler.context.i32_type().into()], true);
                compiler
                    .module
                    .add_function(&op_name, bool_op_fn_type, None)
            }
        };

        let arg_count = compiler
            .context
            .i32_type()
            .const_int(self.values.len() as u64, false);

        let mut args: Vec<_> = self
            .values
            .iter()
            .map(|arg| {
                let codegen_res = arg.generic_codegen(compiler).unwrap();
                let codegen_res = if codegen_res.is_int_value() {
                    create_object(compiler, codegen_res).expect("Could not create object.")
                } else {
                    codegen_res
                };
                compiler
                    .convert_any_value_to_param_value(codegen_res)
                    .expect("Could not convert value to param.")
            })
            .collect();

        args.insert(
            0,
            compiler
                .convert_any_value_to_param_value(arg_count.as_any_value_enum())
                .unwrap(),
        );

        let result = compiler
            .builder
            .build_call(bool_op_fn, &args, "bool_op_result")
            .expect(format!("Could not perform generic {:?}", self.op).as_str())
            .as_any_value_enum();

        Ok(result)
    }
}

impl LLVMGenericCodegen for ExprCall {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        let args: Vec<_> = self
            .args
            .iter()
            .map(|arg| {
                let arg_codegen = arg.generic_codegen(compiler).unwrap();
                if arg_codegen.is_int_value() {
                    create_object(compiler, arg_codegen).expect("Could not create object.")
                } else {
                    arg_codegen
                }
            })
            .collect();

        if let Some(attr) = self.func.as_attribute_expr() {
            if self.args.len() != 1 {
                return Err(BackendError {
                    message: format!(
                        "Incorrect number of arguments given to function {:?}.",
                        attr.attr.as_str()
                    ),
                });
            }
            let value_obj = attr.value.generic_codegen(compiler)?;
            let appended_val = args[0];
            let object_append_fn = compiler
                .module
                .get_function("object_append")
                .expect("object_append is not defined.");
            let params: Vec<_> = Vec::from([value_obj, appended_val])
                .into_iter()
                .map(|arg| {
                    compiler
                        .convert_any_value_to_param_value(arg)
                        .expect("Could not convert value to param.")
                })
                .collect();
            let object_append_call = compiler
                .builder
                .build_call(object_append_fn, &params, "")
                .expect("Could not call object_append.");
            return Ok(object_append_call.as_any_value_enum());
        }

        let func_name = self
            .func
            .as_name_expr()
            .expect("You can only call functions...?")
            .id
            .as_str();
        let function;
        if func_name.eq("print") {
            return build_print_obj_call(compiler, &args);
        } else if func_name.eq("range") {
            return build_generic_range_call(compiler, &args);
        } else if func_name.eq("len") {
            return build_generic_len_call(compiler, &args);
        } else {
            let generic_fn_name = format!("{}_g", func_name);
            function = compiler
                .module
                .get_function(&generic_fn_name.as_str())
                .expect(format!("Could not find function {generic_fn_name}.").as_str());
        }

        // validate function args
        let arg_count = function.count_params();
        if arg_count != self.args.len() as u32 {
            return Err(BackendError {
                message: format!(
                    "Incorrect number of arguments for function {:?}.",
                    func_name
                ),
            });
        }

        let args: Vec<BasicMetadataValueEnum<'_>> = args
            .into_iter()
            .filter_map(|val| match val {
                AnyValueEnum::PointerValue(..) => Some(BasicMetadataValueEnum::PointerValue(
                    val.into_pointer_value(),
                )),
                _ => None,
            })
            .collect();

        let call = compiler
            .builder
            .build_call(function, &args, "tmpcall")
            .expect("Could not call function.");

        Ok(call.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprName {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        match self.ctx {
            ExprContext::Load | ExprContext::Store => {
                let name = self.id.as_str();
                let sym_table_entry = compiler.sym_table.resolve_variable(name);

                match sym_table_entry {
                    Some((_, ptr)) => {
                        if ptr.is_none() {
                            return Err(BackendError { message: "Variable is defined in this scope but doesn't have a corresponding pointer...?".to_string() });
                        }
                        let load = compiler
                            .builder
                            .build_load(ptr.unwrap().into_pointer_value(), "")
                            .expect("Could not load Object pointer.");
                        return Ok(load.as_any_value_enum());
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

impl LLVMGenericCodegen for ExprConstant {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &mut Compiler<'ctx>) -> IRGenResult<'ir> {
        match &self.value {
            Constant::Float(num) => Ok(num.to_any_type(compiler).as_any_value_enum()),
            Constant::Int(num) => {
                let (sign, digits) = num.to_u64_digits();
                // For now, let's just support numbers up to i64
                let i64_num = if !digits.is_empty() {
                    let value = digits[0] as i64;
                    match sign {
                        malachite_bigint::Sign::Minus => -value,
                        _ => value,
                    }
                } else {
                    0
                };
                Ok(i64_num.to_any_type(compiler).as_any_value_enum())
            }
            Constant::Bool(bool) => {
                let new_bool_fn = compiler
                    .module
                    .get_function("new_bool")
                    .expect("new_bool has not been declared.");
                let bool_val = compiler
                    .context
                    .bool_type()
                    .const_int(u64::from(*bool), false);
                let bool_obj = compiler
                    .builder
                    .build_call(new_bool_fn, &[bool_val.into()], "")
                    .expect("Could not create bool Any object.");
                Ok(bool_obj.as_any_value_enum())
            }
            Constant::Str(str) => Ok(str.to_any_type(compiler).as_any_value_enum()),
            _ => Err(BackendError {
                message: format!("Constant type {:?} not implemented yet...", self.value),
            }),
        }
    }
}
