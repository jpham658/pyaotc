use inkwell::types::AnyTypeEnum;
use inkwell::values::{AnyValue, AnyValueEnum, FloatValue};
use inkwell::{builder::Builder, context::Context, module::Module};
use malachite_bigint;
use rustpython_parser::ast::located::ExprContext;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprConstant, ExprName, Operator, Stmt, StmtAssign, StmtExpr,
    StmtFunctionDef,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;


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
        let f64_type = self.context.f64_type();
        let main = self
            .module
            .add_function("main", f64_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");
        self.builder.position_at_end(main_entry);

        for statement in ast {
            match statement.codegen(&self) {
                Ok(ir) => {}
                Err(e) => {
                    println!("{:?}", e);
                    return;
                }
            }
        }

        let _ = self.builder.build_return(Some(&f64_type.const_float(0.0)));

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

impl LLVMCodeGen for Stmt {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.codegen(compiler),
            Stmt::Assign(stmt_assign) => stmt_assign.codegen(compiler),
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMCodeGen for StmtFunctionDef {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMCodeGen for StmtAssign {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        // Only supports single assignment e.g x = 3
        // Cannot do x, y = 3, 5 yet!
        // Also only supports storing constants for now...
        // but this gets fixed when I do my typing..
        match &self.targets[0] {
            Expr::Name(exprname) => {
                let target_name = exprname.id.as_str();
                let value = &self
                    .value
                    .codegen(compiler)
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
                            target_ptr = compiler
                                .builder
                                .build_alloca(compiler.context.i64_type(), target_name)
                                .expect("Cannot allocate variable {target_name}");
                        }
                        _ => {
                            return Err(BackendError {
                                message: "Assignments not implemented for {value.get_type()}",
                            })
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

impl LLVMCodeGen for ExprName {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self.ctx {
            ExprContext::Load | ExprContext::Store => {
                if let Some(name_ptr) = compiler.sym_table.borrow().get(self.id.as_str()) {
                    let load = compiler.builder
                                                             .build_load(name_ptr.into_pointer_value(), &"load")
                                                             .expect("Could not build load instruction.");
                    return Ok(load.as_any_value_enum());
                } else {
                    return Err(BackendError {
                        message: "Variable {self.id.as_str()} doesn't exist in this scope.",
                    });
                }
            }
            ExprContext::Del => Err(BackendError {
                message: "Deleting a variable is not implemented yet.",
            }),
        }
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
            Expr::Name(name) => name.codegen(compiler),
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
        let res = match op {
            Operator::Add => {
                println!("{}", left.get_type());
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
