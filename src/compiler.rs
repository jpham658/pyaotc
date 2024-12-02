use inkwell::llvm_sys::LLVMType;
use inkwell::types::{
    AnyType, AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicType, PointerType,
};
use inkwell::values::{
    AnyValue, AnyValueEnum, ArrayValue, AsValueRef, BasicMetadataValueEnum, BasicValue,
    BasicValueEnum, FloatValue, IntValue, MetadataValue,
};
use inkwell::AddressSpace;
use inkwell::{builder::Builder, context::Context, module::Module};
use malachite_bigint;
use rustpython_parser::ast::{
    Constant, Expr, ExprBinOp, ExprBoolOp, ExprCall, ExprConstant, ExprContext, ExprIfExp,
    ExprList, ExprName, ExprUnaryOp, Operator, Stmt, StmtAssign, StmtExpr, StmtFunctionDef,
    StmtReturn,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use crate::type_inference::{ConcreteValue, Scheme, Type};

#[derive(Debug)]
pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub sym_table: RefCell<HashMap<String, AnyValueEnum<'ctx>>>,
    pub func_args: RefCell<HashMap<String, AnyValueEnum<'ctx>>>,
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
            func_args: RefCell::new(HashMap::new()),
        }
    }

    pub fn compile_with_types(&self, ast: &[Stmt], types: &HashMap<String, Type>) {
        let i32_type = self.context.i32_type();
        self.setup_compiler();

        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");

        self.builder.position_at_end(main_entry);

        for statement in ast {
            match statement.typed_codegen(&self, &types) {
                Ok(_ir) => {}
                Err(e) => {
                    println!("{:?}", e);
                    return;
                }
            }
        }

        let _ = self
            .builder
            .build_return(Some(&i32_type.const_int(0, false)));

        self.dump_module();

        let output = Path::new("outputs/output.ll");

        match self.module.print_to_file(output) {
            Ok(..) => println!(".ll file found at {}", output.display()),
            Err(e) => println!("Could not generate .ll file: {}", e),
        }
    }

    pub fn compile(&self, ast: &[Stmt]) {
        let i32_type = self.context.i32_type();
        self.setup_compiler();

        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");

        self.builder.position_at_end(main_entry);

        for statement in ast {
            match statement.codegen(&self) {
                Ok(_ir) => {}
                Err(e) => {
                    println!("{:?}", e);
                    return;
                }
            }
        }

        let _ = self
            .builder
            .build_return(Some(&i32_type.const_int(0, false)));

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

    pub fn lookup_variable(&self, name: &str) -> Option<AnyValueEnum<'ctx>> {
        if let Ok(func_args) = self.func_args.try_borrow() {
            if let Some(arg) = func_args.get(name) {
                return Some(arg.clone());
            }
        }
        if let Ok(local_vars) = self.sym_table.try_borrow() {
            if let Some(var) = local_vars.get(name) {
                return Some(var.clone());
            }
        }
        None
    }

    fn setup_compiler(&self) {
        let printf_param_types = BasicMetadataTypeEnum::PointerType(
            self.context.i8_type().ptr_type(AddressSpace::default()),
        );
        let printf_type = self.context.i32_type().fn_type(&[printf_param_types], true);
        let printf_func = self.module.add_function("printf", printf_type, None);
        // TODO: define an Any type struct that can be used in generic cases
        let any_type = self.context.struct_type(&[], false);
    }
}

#[derive(Debug, PartialEq)]
pub struct BackendError {
    message: &'static str,
}

type IRGenResult<'ir> = Result<AnyValueEnum<'ir>, BackendError>;

pub trait LLVMCodeGen {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir>;
}

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
            Stmt::Expr(StmtExpr { value, .. }) => value.codegen(compiler),
            Stmt::Assign(assign) => assign.codegen(compiler),
            Stmt::Return(return_stmt) => match &return_stmt.value {
                None => Err(BackendError {
                    message: "Not implemented return by itself yet",
                }),
                Some(expr) => expr.codegen(compiler),
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
                    _ => arg_types.push(*func.input.clone())
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

        // make map for argument types
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

        Ok(func_def.as_any_value_enum())
    }
}

impl LLVMCodeGen for Stmt {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Stmt::Expr(StmtExpr { value, .. }) => value.codegen(compiler),
            Stmt::Assign(assign) => assign.codegen(compiler),
            // Stmt::FunctionDef(funcdef) => funcdef.codegen(compiler), BROKEN FOR NOW...
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
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

impl LLVMCodeGen for Expr {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        match self {
            Expr::BinOp(binop) => binop.codegen(compiler),
            Expr::Constant(constant) => constant.codegen(compiler),
            Expr::Name(name) => name.codegen(compiler),
            Expr::Call(call) => call.codegen(compiler),
            Expr::BoolOp(boolop) => boolop.codegen(compiler),
            Expr::UnaryOp(unop) => unop.codegen(compiler),
            Expr::IfExp(ifexp) => ifexp.codegen(compiler),
            Expr::List(list) => list.codegen(compiler),
            _ => Err(BackendError {
                message: "Not implemented yet...",
            }),
        }
    }
}

impl LLVMCodeGen for ExprList {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMCodeGen for ExprIfExp {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMCodeGen for ExprUnaryOp {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        Err(BackendError {
            message: "Not implemented yet...",
        })
    }
}

impl LLVMCodeGen for ExprBoolOp {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
        let values = self
            .values
            .clone()
            .into_iter()
            .map(|val| val.codegen(compiler).unwrap())
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

impl LLVMCodeGen for ExprCall {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
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
            .map(|arg| arg.codegen(compiler))
            .collect::<Result<Vec<_>, BackendError>>()?;

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

impl LLVMCodeGen for ExprName {
    fn codegen<'ctx: 'ir, 'ir>(&self, compiler: &Compiler<'ctx>) -> IRGenResult<'ir> {
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
