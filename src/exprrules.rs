use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Formatter};
use malachite_bigint;
use inkwell::builder::BuilderError;
use inkwell::values::{AnyValue, AnyValueEnum};
use inkwell::{builder::Builder, context::Context, module::Module};
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprConstant, ExprName, Operator, Stmt, StmtExpr, StmtFunctionDef,
};

// TODO: I need to figure out how to just output the ir for a simple arithmetic
// expression

#[derive(Debug)]
pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub sym_table: RefCell<HashMap<String, AnyValueEnum<'ctx>>>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("python_module");

        Self {
            context,
            builder,
            module,
            sym_table: RefCell::new(HashMap::new()),
        }
    }

    pub fn dump_module(&self) {
        println!(
            "LLVM IR Representation:\n{}",
            self.module.print_to_string().to_string(),
        );
    }
}

#[derive(Debug)]
pub struct BackendError {
    message: &'static str,
}

type IRGenResult<'ir> = Result<AnyValueEnum<'ir>, BackendError>;

pub trait LLVMCodeGen {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir>;
}

impl LLVMCodeGen for StmtFunctionDef {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMCodeGen for ExprConstant {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match &self.value {
            Constant::Float(num) => {
                let f64_type = compiler.context.f64_type();
                Ok(f64_type.const_float(*num).as_any_value_enum())
            }
            Constant::Int(num) => {
                let i64_type = compiler.context.i64_type();
                let (sign, digits) = num.to_u64_digits();
                // For now, let's just support numbers up to i64
                let int_val = digits[0] as u64;
                let is_minus = match sign {
                    malachite_bigint::Sign::Minus => true,
                    _ => false
                };
                Ok(i64_type.const_int(int_val, 
                    is_minus
                ).as_any_value_enum())
            }
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMCodeGen for Expr {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Expr::BinOp(binop) => binop.codegen(compiler),
            Expr::Constant(constant) => constant.codegen(compiler),
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMCodeGen for Stmt {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.codegen(compiler),
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMCodeGen for ExprBinOp {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        // TODO: implement for nested binops
        // Only deals with 1-level arithmetic expressions
        // e.g 3 + 4, 5 + 1.0
        let op = self.op;
        let left = self.left.codegen(compiler)?;
        let right = self.right.codegen(compiler)?;
        let f64_type = compiler.context.f64_type();
        let res = match op {
            Operator::Add => {
                if left.is_int_value() && right.is_int_value() {
                    compiler
                        .builder
                        .build_int_add(left.into_int_value(), right.into_int_value(), &"add")
                        .expect("Could not perform int addition")
                        .as_any_value_enum()
                } else {
                    let lhs;
                    let rhs;
                    match (left, right) {
                        (AnyValueEnum::IntValue(left_int), _) => {
                            println!("Left is int: {}", left_int);
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

                    compiler
                        .builder
                        .build_float_add(
                            lhs.expect("Left operand could not be converted."),
                            rhs.expect("Right operand could not be converted."),
                            &"fadd")
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
                    let lhs;
                    let rhs;
                    match (left, right) {
                        (AnyValueEnum::IntValue(left_int), _) => {
                            println!("Left is int: {}", left_int);
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

                    lhs.as_ref().inspect(|x| println!("LHS: {x}"));
                    rhs.as_ref().inspect(|x| println!("RHS: {x}"));

                    compiler
                        .builder
                        .build_float_sub(
                            lhs.expect("Left operand could not be converted."),
                            rhs.expect("Right operand could not be converted."),
                            &"fsub",
                        )
                        .expect("Could not perform float subtraction")
                        .as_any_value_enum()
                }
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

enum ConstantType {
    None,
    Bool,
    Str,
    Bytes,
    Int,
    Tuple,
    Float,
    Complex,
    Ellipsis,
}

pub struct Heuristic {
    typename: ConstantType,
    score: f64,
}

impl Debug for Heuristic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Heuristic")
            .field("typename", &self.typename)
            .field("score", &self.score)
            .finish()
    }
}

impl Debug for ConstantType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ConstantType::None => write!(f, "None"),
            ConstantType::Bool => write!(f, "Bool"),
            ConstantType::Str => write!(f, "Str"),
            ConstantType::Float => write!(f, "Float"),
            ConstantType::Int => write!(f, "Int"),
            ConstantType::Tuple => write!(f, "Tuple"),
            ConstantType::Ellipsis => write!(f, "Ellipsis"),
            ConstantType::Bytes => write!(f, "Bytes"),
            ConstantType::Complex => write!(f, "Complex"),
        }
    }
}
