use std::collections::HashMap;

use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;
use malachite_bigint;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprCompare, ExprConstant, ExprContext,
    ExprIfExp, ExprList, ExprName, ExprUnaryOp, Stmt, StmtAssign, StmtExpr, StmtFunctionDef,
    UnaryOp,
};

use crate::compiler::Compiler;
use crate::compiler_utils::print_fn::build_print_bool_fn;
use crate::compiler_utils::to_any_type::ToAnyType;

use super::any_class_utils::{cast_any_to_struct, get_value};
use super::error::{BackendError, IRGenResult};
use super::generic_ops::cmp_operators::build_generic_cmp_op::build_generic_cmp_op;
use super::generic_ops::operators::build_generic_op::build_generic_op;

pub trait LLVMGenericCodegen {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir>;
}

impl LLVMGenericCodegen for ExprBinOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let left = self.left.generic_codegen(compiler)?;
        let right = self.right.generic_codegen(compiler)?;
        let g_op_fn_name = format!("{:?}", self.op);
        let g_op_opt = compiler.module.get_function(&g_op_fn_name);
        let g_op_fn = if let Some(fn_val) = g_op_opt {
            fn_val
        } else {
            build_generic_op(&self.op, compiler).expect("Could not build generic op function.")
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
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.generic_codegen(compiler),
            Stmt::Assign(assign) => assign.generic_codegen(compiler),
            Stmt::Return(return_stmt) => match &return_stmt.value {
                None => Err(BackendError {
                    message: "Not implemented return by itself yet",
                }),
                Some(expr) => expr.generic_codegen(compiler),
            },
            Stmt::FunctionDef(funcdef) => funcdef.generic_codegen(compiler),
            _ => Err(BackendError {
                message: "Not implemented yet...",
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
        let obj_ptr = compiler.object_type.ptr_type(AddressSpace::default());
        let any_args: Vec<BasicMetadataTypeEnum<'_>> = self
            .args
            .args
            .clone()
            .into_iter()
            .map(|_a| obj_ptr.into())
            .collect();

        // Declare function with Any return type
        let func_type = obj_ptr.fn_type(&any_args, false);
        let func_def = compiler
            .module
            .add_function(func_name.as_str(), func_type, None);

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
            .map(|(k, v)| (k.clone(), v.as_any_value_enum()))
            .collect::<HashMap<_, _>>();

        {
            let mut func_args = compiler.func_args.borrow_mut();
            func_args.extend(arg_map.clone());
        }

        let func_entry = compiler.context.append_basic_block(func_def, "entry");
        compiler.builder.position_at_end(func_entry);
        let mut return_stmts = Vec::new();

        // build function body
        for statement in &self.body {
            let res = statement.generic_codegen(compiler);
            match res {
                Err(e) => return Err(e),
                Ok(ir) => match statement {
                    Stmt::Return(..) => {
                        return_stmts.push(ir);
                    }
                    _ => {}
                },
            }
        }

        if return_stmts.is_empty() {
            let _ = compiler.builder.build_return(None);
        } else {
            let _ = compiler
                .builder
                .build_return(Some(&return_stmts[0].into_pointer_value()));
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
                let target_ptr = if !sym_table.contains_key(target_name) {
                    // All generic objects are represented as pointers Object*
                    if value.is_int_value() {
                        compiler
                            .builder
                            .build_alloca(compiler.context.bool_type(), "")
                            .expect("Could not create pointer for bool type.")
                    } else {
                        compiler
                            .builder
                            .build_alloca(compiler.object_type.ptr_type(AddressSpace::default()), "")
                            .expect("Could not create pointer for Object type.")
                    }
                } else {
                    sym_table.get(target_name).unwrap().into_pointer_value()
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
                sym_table.insert(target_name.to_string(), target_ptr.as_any_value_enum());
                Ok(store.as_any_value_enum())
            }
            _ => Err(BackendError {
                message: "Left of an assignment must be a variable.",
            }),
        }
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
            let g_cmpop_opt = compiler.module.get_function(&g_cmpop_fn_name);
            let g_op_fn = if let Some(fn_val) = g_cmpop_opt {
                fn_val
            } else {
                build_generic_cmp_op(&op, compiler)
                    .expect("Could not build generic cmp-op function.")
            };
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
                .expect(format!("Could not perform generic {:?}.", op).as_str());
            conditions.push(llvm_comp.as_any_value_enum());
            left = llvm_comp.as_any_value_enum();
        }

        let mut composite_comp = conditions[0].into_int_value();
        
        for cond in conditions {
            // Result from comparisons are always booleans, so store them as such
            let cond = cond.into_int_value();
            composite_comp = compiler
                .builder
                .build_and(composite_comp, cond, "")
                .expect("Could not build 'and' for compare.");
        }

        Ok(composite_comp.as_any_value_enum())
    }
}

// TODO: Since I know the result of this will either be int or float,
// do I even need to wrap in Any?
impl LLVMGenericCodegen for ExprUnaryOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let bool_type = compiler.context.bool_type();
        let truth_val = bool_type.const_int(u64::from(true), false);
        let false_val = bool_type.const_int(u64::from(false), false);

        let i64_type = compiler.context.i64_type();
        let zero = i64_type.const_zero();

        // TODO: Update this with generic operator functions...
        let operand_ptr = self.operand.generic_codegen(compiler)?.into_pointer_value();
        let operand = get_value(operand_ptr, compiler).as_any_value_enum();

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
        let mut function;
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
        if arg_count != self.args.len() as u32 {
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

        let args: Vec<BasicMetadataValueEnum<'_>> = args
            .into_iter()
            .filter_map(|val| match val {
                AnyValueEnum::PointerValue(..) => Some(BasicMetadataValueEnum::PointerValue(
                    val.into_pointer_value(),
                )),
                AnyValueEnum::IntValue(..) => {
                    if func_name.eq("print") {
                        function = if let Some(func) = compiler.module.get_function("print_bool") {
                            func
                        } else {
                            build_print_bool_fn(compiler)
                        }
                    };
                    Some(BasicMetadataValueEnum::IntValue(
                        val.into_int_value(),
                    ))
                },
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
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self.ctx {
            ExprContext::Load | ExprContext::Store => {
                let name = self.id.as_str();
                let func_args = compiler.func_args.borrow();
                if let Some(arg_val) = func_args.get(name) {
                    return Ok(arg_val.clone());
                }
                let sym_table = compiler.sym_table.borrow();
                if let Some(name_ptr) = sym_table.get(name) {
                    // TODO: Change after we store ptrs of ptrs in sym table
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
            // Constant::Str(str) => {}
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}
