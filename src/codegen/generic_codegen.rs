use std::collections::HashMap;

use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;
use malachite_bigint;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprCompare, ExprConstant, ExprContext,
    ExprIfExp, ExprList, ExprName, ExprUnaryOp, Stmt, StmtAssign, StmtExpr, StmtFunctionDef,
    StmtIf, StmtWhile,
};

use crate::astutils::GetReturnStmts;
use crate::compiler::Compiler;
use crate::compiler_utils::to_any_type::ToAnyType;

use super::error::{BackendError, IRGenResult};

pub trait LLVMGenericCodegen {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir>;
}

impl LLVMGenericCodegen for ExprBinOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let left = self.left.generic_codegen(compiler)?;
        let right = self.right.generic_codegen(compiler)?;
        let g_op_fn_name = format!("{:?}", self.op);
        let g_op_fn = compiler.module.get_function(&g_op_fn_name).unwrap();
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
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.generic_codegen(compiler),
            Stmt::Assign(assign) => assign.generic_codegen(compiler),
            Stmt::If(ifstmt) => ifstmt.generic_codegen(compiler),
            Stmt::While(whilestmt) => whilestmt.generic_codegen(compiler),
            Stmt::Return(return_stmt) => match &return_stmt.value {
                None => {
                    let ret_none = compiler
                        .builder
                        .build_return(None)
                        .expect("Could not build void return.");
                    Ok(ret_none.as_any_value_enum())
                }
                Some(expr) => expr.generic_codegen(compiler),
            },
            Stmt::FunctionDef(funcdef) => funcdef.generic_codegen(compiler),
            _ => Err(BackendError {
                message: "Codegen for statement type not implemented yet...",
            }),
        }
    }
}

impl LLVMGenericCodegen for StmtFunctionDef {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        // All arguments are generic Any containers
        // So generate code for function body as generic as possible
        let main_entry = compiler
            .builder
            .get_insert_block()
            .expect("Builder isn't mapped to a basic block?");

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
        // -- TODO: Return bool type if we know that the type will be bool (i.e. we have a compare)? --
        let return_stmts = self.clone().get_return_stmts();
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
            func_args.extend(arg_map.clone());
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
                    message: "Return statement cannot be declared twice in function body.",
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
            let mut func_args = compiler.func_args.borrow_mut();
            func_args.clear();
        }

        Ok(func_def.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for StmtAssign {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match &self.targets[0] {
            Expr::Name(exprname) => {
                let target_name = exprname.id.as_str();
                let value = &self
                    .value
                    .generic_codegen(compiler)
                    .expect("Failed to compute right side of assignment.");
                let mut sym_table = compiler.sym_table.borrow_mut();
                let func_args = compiler.func_args.borrow();
                let target_ptr = if func_args.contains_key(target_name) {
                    func_args.get(target_name).unwrap().into_pointer_value()
                } else if sym_table.contains_key(target_name) {
                    sym_table.get(target_name).unwrap().into_pointer_value()
                } else {
                    // All generic objects are represented as pointers Object*
                    if value.is_int_value() {
                        compiler
                            .builder
                            .build_alloca(compiler.context.bool_type(), "")
                            .expect("Could not create pointer for bool type.")
                    } else {
                        compiler
                            .builder
                            .build_alloca(
                                compiler.object_type.ptr_type(AddressSpace::default()),
                                "",
                            )
                            .expect("Could not create pointer for Object type.")
                    }
                };

                // Value will either definitely be a boolean, or Object*
                let store = if value.is_int_value() {
                    compiler
                        .builder
                        .build_store(target_ptr, value.into_int_value())
                        .expect("Could not store bool value.")
                } else {
                    compiler
                        .builder
                        .build_store(target_ptr, value.into_pointer_value())
                        .expect("Could not store Any value.")
                };

                if !func_args.contains_key(target_name) {
                    sym_table.insert(target_name.to_string(), target_ptr.as_any_value_enum());
                }

                Ok(store.as_any_value_enum())
            }
            _ => Err(BackendError {
                message: "Left of an assignment must be a variable.",
            }),
        }
    }
}

impl LLVMGenericCodegen for StmtWhile {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
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
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
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
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Expr::BinOp(binop) => binop.generic_codegen(compiler),
            Expr::Constant(constant) => constant.generic_codegen(compiler),
            Expr::Name(name) => name.generic_codegen(compiler),
            Expr::Call(call) => call.generic_codegen(compiler),
            Expr::BoolOp(boolop) => boolop.generic_codegen(compiler),
            Expr::Compare(cmp) => cmp.generic_codegen(compiler),
            Expr::UnaryOp(unop) => unop.generic_codegen(compiler),
            Expr::IfExp(ifexp) => ifexp.generic_codegen(compiler),
            Expr::List(list) => list.generic_codegen(compiler),
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMGenericCodegen for ExprList {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMGenericCodegen for ExprIfExp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMGenericCodegen for ExprCompare {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let mut left = self.left.generic_codegen(compiler)?;
        let comparators = self
            .comparators
            .clone()
            .into_iter()
            .map(|comp| {
                comp.generic_codegen(compiler)
                    .expect("Could not compile comparator.")
            })
            .collect::<Vec<_>>();
        let ops = self.ops.clone();

        let op_and_comp = ops
            .into_iter()
            .zip(comparators.into_iter())
            .collect::<Vec<(_, _)>>();

        let mut conditions = Vec::new();

        for (op, comp) in op_and_comp {
            if comp.is_vector_value() {
                return Err(BackendError {
                    message: "Invalid type for right compare value.",
                });
            }
            let g_cmpop_fn_name = format!("{:?}", op);
            let g_op_fn = compiler.module.get_function(&g_cmpop_fn_name).unwrap();
            let llvm_comp = compiler
                .builder
                .build_call(
                    g_op_fn,
                    &[
                        BasicMetadataValueEnum::PointerValue(left.into_pointer_value()),
                        BasicMetadataValueEnum::PointerValue(comp.into_pointer_value()),
                    ],
                    "",
                )
                .expect(format!("Could not perform generic {:?}.", op).as_str())
                .as_any_value_enum();
            conditions.push(llvm_comp.into_int_value());
            left = llvm_comp.as_any_value_enum();
        }

        let mut composite_comp = conditions[0];

        for cond in conditions {
            // Result from comparisons are always booleans, so store them as such
            let cond = cond;
            composite_comp = compiler
                .builder
                .build_and(composite_comp, cond, "")
                .expect("Could not build 'and' for compare.");
        }

        Ok(composite_comp.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprUnaryOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let g_uop_name = format!("{:?}", self.op);
        let g_uop_fn = compiler.module.get_function(&g_uop_name).unwrap();

        let operand_obj = self.operand.generic_codegen(compiler)?.into_pointer_value();
        let g_uop_res = compiler
            .builder
            .build_call(g_uop_fn, &[operand_obj.into()], "")
            .expect(format!("Failed to call {}", g_uop_name).as_str());
        Ok(g_uop_res.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprBoolOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let values = self
            .values
            .clone()
            .into_iter()
            .map(|val| val.generic_codegen(compiler).unwrap())
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

impl LLVMGenericCodegen for ExprCall {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let func_name = self
            .func
            .as_name_expr()
            .expect("You can only call functions...?")
            .id
            .as_str();
        let function;
        if func_name.eq("print") {
            function = compiler.module.get_function("print_obj").unwrap();
        } else {
            let generic_fn_name = format!("{}_g", func_name);
            function = compiler
                .module
                .get_function(&generic_fn_name.as_str())
                .expect("Could not find function.");
        }

        // validate function args
        let arg_count = function.count_params();
        if !func_name.eq("print") && arg_count != self.args.len() as u32 {
            return Err(BackendError {
                message: "Incorrect number of arguments provided.",
            });
        }

        // generic_codegen args if we have any
        let args = self
            .args
            .iter()
            .map(|arg| arg.generic_codegen(compiler))
            .collect::<Result<Vec<_>, BackendError>>()?;

        let mut args: Vec<BasicMetadataValueEnum<'_>> = args
            .into_iter()
            .filter_map(|val| match val {
                AnyValueEnum::PointerValue(..) => Some(BasicMetadataValueEnum::PointerValue(
                    val.into_pointer_value(),
                )),
                AnyValueEnum::IntValue(i) => {
                    // Shouldn't fail bc it was in our setup IR!
                    let new_bool = compiler.module.get_function("new_bool").unwrap();
                    let bool_obj = compiler
                        .builder
                        .build_call(new_bool, &[i.into()], "")
                        .expect("Could not call new_bool.")
                        .as_any_value_enum();
                    Some(BasicMetadataValueEnum::PointerValue(
                        bool_obj.into_pointer_value(),
                    ))
                }
                _ => None,
            })
            .collect();

        if func_name.eq("print") {
            let llvm_arg_count = compiler
                .context
                .i32_type()
                .const_int(args.len() as u64, false);
            args.insert(0, BasicMetadataValueEnum::IntValue(llvm_arg_count));
        }

        let call = compiler
            .builder
            .build_call(function, &args, "tmpcall")
            .expect("Could not call function.");

        Ok(call.as_any_value_enum())
    }
}

impl LLVMGenericCodegen for ExprName {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self.ctx {
            ExprContext::Load | ExprContext::Store => {
                let name = self.id.as_str();
                let func_args = compiler.func_args.borrow();
                if let Some(arg_ptr) = func_args.get(name) {
                    let load = compiler
                        .builder
                        .build_load(arg_ptr.into_pointer_value(), "")
                        .expect("Could not load Object pointer.");
                    return Ok(load.as_any_value_enum());
                }
                let sym_table_as_any = compiler.sym_table_as_any.borrow();
                if let Some(name_ptr) = sym_table_as_any.get(name) {
                    return Ok(name_ptr.as_any_value_enum());
                }
                let sym_table = compiler.sym_table.borrow();
                if let Some(name_ptr) = sym_table.get(name) {
                    let load = compiler
                        .builder
                        .build_load(name_ptr.into_pointer_value(), "")
                        .expect("Could not load Object pointer.");
                    return Ok(load.as_any_value_enum());
                }
                Err(BackendError {
                    message: "Variable {name} is not defined.",
                })
            }
            ExprContext::Del => Err(BackendError {
                message: "Deleting a variable is not implemented yet.",
            }),
        }
    }
}

impl LLVMGenericCodegen for ExprConstant {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
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
                message: "Not implemented yet...",
            }),
        }
    }
}
