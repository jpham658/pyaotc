use inkwell::values::{AnyValue, AnyValueEnum, FloatValue};
use inkwell::{builder::Builder, context::Context, module::Module};
use malachite_bigint;
use rustpython_parser::ast::located::ExprContext;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprConstant, ExprName, Operator, Stmt, StmtExpr, StmtFunctionDef,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::process::Command;

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

    pub fn compile(&self, ast: Vec<Stmt>) {
        let i64_type = self.context.i64_type();
        let f64_type = self.context.f64_type();
        let main = self
            .module
            .add_function("main", f64_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(main_entry);

        let mut last_val = None;

        for statement in ast {
            match statement.codegen(&self) {
                Ok(ir) => {
                    last_val = Some(ir); // Store last evaluated IR value
                }
                Err(e) => {
                    println!("{:?}", e);
                    return;
                }
            }
        }

        if let Some(final_val) = last_val {
            // Return the final evaluated value, assuming it’s a float to cast to i64
            match final_val {
                AnyValueEnum::FloatValue(float_val) => {
                    let _ = self.builder.build_return(Some(&float_val));
                }
                AnyValueEnum::IntValue(int_val) => {
                    let _ = self.builder.build_return(Some(&int_val));
                }
                _ => {}
            }
        }

        self.dump_module();

        let output = Path::new("outputs/output.ll");

        match self.module.print_to_file(output) {
            Ok(..) => println!(".ll file found at {}", output.display()),
            Err(e) => println!("Could not generate .ll file: {}", e),
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

impl LLVMCodeGen for ExprName {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self.ctx {
            ExprContext::Load => println!("We are loading in variable {}", self.id),
            ExprContext::Store => println!("We are storing in variable {}", self.id),
            ExprContext::Del => println!("We are deleting variable {}", self.id)
        }
        Err(BackendError {
            message: &"Not implemented yet...",
        })
    }
}

impl LLVMCodeGen for ExprConstant {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        println!("Current Block: {:?}", compiler.builder.get_insert_block());

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
                    _ => false,
                };
                Ok(i64_type.const_int(int_val, is_minus).as_any_value_enum())
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
        println!("Current Block: {:?}", compiler.builder.get_insert_block());

        let op = self.op;
        let left = self.left.codegen(compiler)?;
        let right = self.right.codegen(compiler)?;
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
        println!("Generated IR: {:?}", res);
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
    float_map.insert(left, lhs);
    float_map.insert(right, rhs);
    float_map
}
