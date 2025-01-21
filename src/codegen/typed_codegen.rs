use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, FloatValue};
use inkwell::AddressSpace;
use malachite_bigint;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprConstant, ExprContext, ExprIfExp,
    ExprList, ExprName, ExprUnaryOp, Operator, Stmt, StmtAssign, StmtExpr, StmtFunctionDef,
};

use std::collections::HashMap;

use crate::compiler_utils::print_fn::print_fn;
use crate::compiler_utils::to_any_type::ToAnyType;
use crate::type_inference::{ConcreteValue, Scheme, Type};
use crate::compiler::Compiler;

use super::error::{BackendError, IRGenResult};

pub trait LLVMTypedCodegen {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &Compiler<'ctx>,
        types: &HashMap<String, Type>,
    ) -> IRGenResult<'ir>;
}

impl LLVMTypedCodegen for Stmt {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &Compiler<'ctx>,
        types: &HashMap<String, Type>,
    ) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.typed_codegen(compiler, types),
            Stmt::Assign(assign) => assign.typed_codegen(compiler, types),
            Stmt::Return(return_stmt) => match &return_stmt.value {
                None => Err(BackendError {
                    message: "Not implemented return by itself yet",
                }),
                Some(expr) => expr.typed_codegen(compiler, types),
            },
            Stmt::FunctionDef(funcdef) => funcdef.typed_codegen(compiler, types),
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMTypedCodegen for StmtFunctionDef {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &Compiler<'ctx>,
        types: &HashMap<String, Type>,
    ) -> IRGenResult<'ir> {
        // save main entry point
        let main_entry = compiler
            .builder
            .get_insert_block()
            .expect("Builder isn't mapped to a basic block?");

        let func_name = self.name.as_str();
        let func_type;
        match types.get(func_name) {
            Some(typ) => func_type = typ,
            None => {
                return Err(BackendError {
                    message: "Function {func_name} not typed.",
                })
            }
        }

        // get argument and return types
        let mut arg_types: Vec<Type> = Vec::new();
        let return_type: Type;
        match &func_type {
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

        let llvm_arg_types: Vec<BasicMetadataTypeEnum> = arg_types
            .into_iter()
            .map(|arg_type| {
                match arg_type {
                    Type::ConcreteType(ConcreteValue::Int) => compiler.context.i64_type().into(),
                    Type::ConcreteType(ConcreteValue::Float) => compiler.context.f64_type().into(),
                    _ => {
                        compiler.context.i8_type().into() // TODO: better way to handle this
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
            Type::ConcreteType(ConcreteValue::Bool) | Type::ConcreteType(ConcreteValue::Str) => {
                compiler.context.i8_type().fn_type(&llvm_arg_types, false)
            }
            Type::ConcreteType(ConcreteValue::None) => {
                compiler.context.void_type().fn_type(&llvm_arg_types, false)
            }
            Type::Scheme(Scheme { type_name, .. }) => match **type_name {
                Type::ConcreteType(ConcreteValue::Int) => {
                    compiler.context.i64_type().fn_type(&llvm_arg_types, false)
                }
                Type::ConcreteType(ConcreteValue::Float) => {
                    compiler.context.f64_type().fn_type(&llvm_arg_types, false)
                }
                Type::ConcreteType(ConcreteValue::Bool)
                | Type::ConcreteType(ConcreteValue::Str) => {
                    compiler.context.i8_type().fn_type(&llvm_arg_types, false)
                }
                _ => {
                    println!("{:?}", return_type);
                    return Err(BackendError {
                        message: "Not a valid function return type after visiting scheme.",
                    });
                }
            },
            _ => {
                println!("{:?}", return_type);
                return Err(BackendError {
                    message: "Not a valid function return type.",
                });
            }
        };

        let func_def = compiler
            .module
            .add_function(func_name, llvm_return_type, None);

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
            let res = statement.typed_codegen(compiler, types);
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

        // assume for now we only have one return statement at the end of our function
        match &return_type {
            Type::ConcreteType(ConcreteValue::Int) => {
                let _ = compiler
                    .builder
                    .build_return(Some(&return_stmts[0].into_int_value()));
            }
            Type::ConcreteType(ConcreteValue::Float) => {
                let _ = compiler
                    .builder
                    .build_return(Some(&return_stmts[0].into_float_value()));
            }
            Type::ConcreteType(ConcreteValue::Bool) => {
                let _ = compiler
                    .builder
                    .build_return(Some(&return_stmts[0].into_int_value()));
            }
            Type::ConcreteType(ConcreteValue::Str) => {
                let _ = compiler
                    .builder
                    .build_return(Some(&return_stmts[0].into_pointer_value()));
            }

            Type::Scheme(Scheme { type_name, .. }) => match **type_name {
                Type::ConcreteType(ConcreteValue::Int) => {
                    let _ = compiler
                        .builder
                        .build_return(Some(&return_stmts[0].into_int_value()));
                }
                Type::ConcreteType(ConcreteValue::Float) => {
                    let _ = compiler
                        .builder
                        .build_return(Some(&return_stmts[0].into_float_value()));
                }
                Type::ConcreteType(ConcreteValue::Bool) => {
                    let _ = compiler
                        .builder
                        .build_return(Some(&return_stmts[0].into_int_value()));
                }
                Type::ConcreteType(ConcreteValue::Str) => {
                    let _ = compiler
                        .builder
                        .build_return(Some(&return_stmts[0].into_pointer_value()));
                }
                _ => {
                    println!("{:?}", return_type);
                    return Err(BackendError {
                        message: "Not a valid function return type after visiting scheme.",
                    });
                }
            },
            _ => {
                return Err(BackendError {
                    message: "Not a valid function return type.",
                })
            }
        };

        // reset back to normal!
        compiler.builder.position_at_end(main_entry);
        {
            let mut func_args = compiler.func_args.borrow_mut();
            func_args.clear();
        }

        // EXPERIMENTS...
        // let's mimic the casting on an argument, store AnyInt in Any
        let x_any = (30i64).to_any_type(&compiler);
        println!("x's ptr type: {:?}", x_any.get_type());

        // Cast Any to AnyInt if it is indeed storing an int
        // TODO: Make this a separate predefined method (instanceof)
        let any_int_type_ptr = compiler.any_int_type.ptr_type(AddressSpace::default());
        let casted_x_ptr = compiler
            .builder
            .build_bit_cast(x_any, any_int_type_ptr, "casted_to_i") // TODO: Fresh var generator
            .expect("Error: Could not cast Any container pointer to integer variation.");
        println!("Type of ptr after casting: {:?}", casted_x_ptr);

        let x_val_ptr = compiler
            .builder
            .build_struct_gep(casted_x_ptr.into_pointer_value(), 1, "x_val_ptr")
            .expect("Error: Could not get pointer to x's value.");
        let x_val = compiler
            .builder
            .build_load(x_val_ptr, "x_val")
            .expect("Error: Could not load x's value.");
        println!("Type of x's val: {:?}", x_val.get_type());

        Ok(func_def.as_any_value_enum())
    }
}

impl LLVMTypedCodegen for StmtAssign {
    fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
        match &self.targets[0] {
            Expr::Name(exprname) => {
                let target_name = exprname.id.as_str();
                let value = &self
                    .value
                    .typed_codegen(compiler, types)
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

impl LLVMTypedCodegen for Expr {
    fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
        match self {
            Expr::BinOp(binop) => binop.typed_codegen(compiler, types),
            Expr::Constant(constant) => constant.typed_codegen(compiler, types),
            Expr::Name(name) => name.typed_codegen(compiler, types),
            Expr::Call(call) => call.typed_codegen(compiler, types),
            Expr::BoolOp(boolop) => boolop.typed_codegen(compiler, types),
            Expr::UnaryOp(unop) => unop.typed_codegen(compiler, types),
            Expr::IfExp(ifexp) => ifexp.typed_codegen(compiler, types),
            Expr::List(list) => list.typed_codegen(compiler, types),
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMTypedCodegen for ExprList {
        fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMTypedCodegen for ExprIfExp {
        fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMTypedCodegen for ExprUnaryOp {
        fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMTypedCodegen for ExprBoolOp {
        fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
        let values = self
            .values
            .clone()
            .into_iter()
            .map(|val| val.typed_codegen(compiler, types).unwrap())
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

impl LLVMTypedCodegen for ExprCall {
    // TODO: Refactor to consider general and specific types.
        fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
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

        // codegen args if we have any
        let args = self
            .args
            .iter()
            .map(|arg| arg.typed_codegen(compiler, types))
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

impl LLVMTypedCodegen for ExprName {
        fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
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

impl LLVMTypedCodegen for ExprConstant {
        fn typed_codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>, types: &HashMap<String, Type>) -> IRGenResult<'ir> {
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

impl LLVMTypedCodegen for ExprBinOp {
    fn typed_codegen<'ctx: 'ir, 'ir>(
        &self,
        compiler: &Compiler<'ctx>,
        types: &HashMap<String, Type>,
    ) -> IRGenResult<'ir> {
        // TODO: So this part becomes convert whatever left and right are to Any type
        // Then call generic functions like g_add(Any left, Any right), g_sub(Any left, Any right), etc.
        // But, how do I cast AnyValueEnums...?
        let op = self.op;
        let left = self.left.typed_codegen(compiler, types)?;
        let right = self.right.typed_codegen(compiler, types)?;
        let res = match op {
            Operator::Add => {
                if left.is_int_value() && right.is_int_value() {
                    compiler
                        .builder
                        .build_int_add(left.into_int_value(), right.into_int_value(), &"add")
                        .expect("Could not perform int addition")
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
                    lhs = left_val.const_signed_to_float(f64_type);
                } else {
                    lhs = left.into_float_value();
                }
                if let AnyValueEnum::IntValue(right_val) = right {
                    rhs = right_val.const_signed_to_float(f64_type);
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

fn get_left_and_right_as_floats<'ctx>(
    compiler: &Compiler<'ctx>,
    left: AnyValueEnum<'ctx>,
    right: AnyValueEnum<'ctx>,
) -> std::collections::HashMap<AnyValueEnum<'ctx>, Option<FloatValue<'ctx>>> {
    let mut float_map = std::collections::HashMap::new();
    let lhs;
    let rhs;
    let f64_type = compiler.context.f64_type();
    match (left, right) {
        (AnyValueEnum::IntValue(left_int), _) => {
            lhs = Some(left_int.const_signed_to_float(f64_type));
            rhs = Some(right.into_float_value());
        }
        (_, AnyValueEnum::IntValue(right_int)) => {
            lhs = Some(left.into_float_value());
            rhs = Some(right_int.const_signed_to_float(f64_type));
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