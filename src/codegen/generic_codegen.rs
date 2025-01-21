use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use malachite_bigint;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprConstant, ExprContext, ExprIfExp,
    ExprList, ExprName, ExprUnaryOp, Operator, Stmt, StmtAssign, StmtExpr, StmtFunctionDef,
};

use crate::compiler_utils::print_fn::print_fn;
use crate::compiler_utils::to_any_type::ToAnyType;
use crate::compiler::Compiler;

use super::error::{BackendError, IRGenResult};


pub trait LLVMGenericCodegen {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir>;
}

impl LLVMGenericCodegen for ExprBinOp {
    /**
     * Only called when we want to create a generic function.
     */
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let left = self.left.generic_codegen(compiler)?; // Loads Any struct 
        let right = self.right.generic_codegen(compiler)?; // Loads Any struct
        match self.op {
            Operator::Add => {
                // call generic add
                let g_add = compiler.module
                    .get_function("g_add")
                    .expect("Generic addition function has not been declared");
                let g_add_call = compiler.builder.build_call(g_add, &[
                        BasicMetadataValueEnum::StructValue(left.into_struct_value()), 
                        BasicMetadataValueEnum::StructValue(right.into_struct_value())
                    ], "g_add_call") // TODO: Declare g_add function in setup ...
                    .expect("Could not perform generic add.");
                Ok(g_add_call.as_any_value_enum())
            }
            _ => Err(BackendError {
                message: "Not implemented yet...",
            })
        }
    }
}

impl LLVMGenericCodegen for Stmt {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.generic_codegen(compiler),
            Stmt::Assign(assign) => assign.generic_codegen(compiler),
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

        let func_name = format!("g_{}", self.name.as_str());
        let any_args: Vec<BasicMetadataTypeEnum> = self
            .args
            .args
            .clone()
            .into_iter()
            .map(|_a| compiler.any_type.into())
            .collect();

        // Declare function with Any return type
        let func_type = compiler.any_type.fn_type(&any_args, false);
        let func_def = compiler.module.add_function(func_name.as_str(), func_type, None);

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

        compiler.builder.position_at_end(main_entry);

        Err(BackendError {
            message: "Not implemented yet...",
        })
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
                let target_ptr;
                if !sym_table.contains_key(target_name) {
                    match value.get_type() {
                        AnyTypeEnum::FloatType(..) => {
                            target_ptr = compiler
                                .builder
                                .build_alloca(compiler.context.f64_type(), target_name)
                                .expect("Cannot allocate variable {target_name}");
                        }
                        AnyTypeEnum::IntType(..) => {
                            // Booleans get cast to integers, so distinguish between the two...
                            let itype = value.get_type().into_int_type();
                            let alloc_type = if itype == compiler.context.i8_type() {
                                compiler.context.i8_type()
                            } else {
                                compiler.context.i64_type()
                            };
                            target_ptr = compiler
                                .builder
                                .build_alloca(alloc_type, target_name)
                                .expect("Cannot allocate variable {target_name}");
                        }
                        AnyTypeEnum::PointerType(..) => {
                            // Pointers are usually i8 ? Might wanna double check this
                            target_ptr = compiler
                                .builder
                                .build_alloca(value.into_pointer_value().get_type(), target_name)
                                .expect("Cannot allocate variable {target_name}");
                        }
                        _ => {
                            return Err(BackendError {
                                message: "Assignments not implemented for {value.get_type()}",
                            });
                        }
                    }
                } else {
                    // should only be pointers in the symbol table
                    target_ptr = sym_table.get(target_name).unwrap().into_pointer_value();
                }
                let store;

                match value.get_type() {
                    AnyTypeEnum::IntType(..) => {
                        store = compiler
                            .builder
                            .build_store(target_ptr, value.into_int_value())
                            .expect("Could not store variable {target_name}.");
                    }
                    AnyTypeEnum::FloatType(..) => {
                        store = compiler
                            .builder
                            .build_store(target_ptr, value.into_float_value())
                            .expect("Could not store variable {target_name}.");
                    }
                    AnyTypeEnum::PointerType(..) => {
                        // TODO: Check in with this implementation
                        // like how weird is this to do??
                        let ptr_address = value.into_pointer_value();
                        store = compiler
                            .builder
                            .build_store(target_ptr, ptr_address)
                            .expect("Cannot allocate variable {target_name}");
                    }
                    _ => {
                        return Err(BackendError {
                            message: "Assignments not implemented for {value.get_type()}",
                        })
                    }
                }
                sym_table.insert(target_name.to_string(), target_ptr.as_any_value_enum());
                return Ok(store.as_any_value_enum());
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

impl LLVMGenericCodegen for ExprUnaryOp {
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
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

        let true_val = compiler.context.i8_type().const_int(1, false);
        let false_val = compiler.context.i8_type().const_zero();
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
    // TODO: Refactor to consider general and specific types.
    fn generic_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
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
        } else {
            function = compiler
                .module
                .get_function(func_name)
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

        if func_name.eq("print") {
            return print_fn(compiler, &args);
        }

        let args: Vec<BasicMetadataValueEnum> = args
            .into_iter()
            .filter_map(|val| match val {
                AnyValueEnum::IntValue(..) => {
                    Some(BasicMetadataValueEnum::IntValue(val.into_int_value()))
                }
                AnyValueEnum::FloatValue(..) => {
                    Some(BasicMetadataValueEnum::FloatValue(val.into_float_value()))
                }
                AnyValueEnum::PointerValue(..) => Some(BasicMetadataValueEnum::PointerValue(
                    val.into_pointer_value(),
                )),
                AnyValueEnum::StructValue(..) => Some(BasicMetadataValueEnum::StructValue(
                    val.into_struct_value()
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
                    let load = compiler
                        .builder
                        .build_load(name_ptr.into_pointer_value(), name)
                        .expect("Could not load variable.");
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
                let i8_type = compiler.context.i8_type();
                let bool_val = u64::from(*bool);
                Ok(i8_type.const_int(bool_val, false).as_any_value_enum())
            }
            Constant::Str(str) => {
                let str_ptr = compiler
                    .builder
                    .build_global_string_ptr(str, "tmpstr")
                    .expect("Could not create global string ptr for {str}.");
                Ok(str_ptr.as_any_value_enum())
            }
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}