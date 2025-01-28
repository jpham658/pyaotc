use inkwell::types::{BasicMetadataTypeEnum, StructType};
use inkwell::values::AnyValueEnum;
use inkwell::AddressSpace;
use inkwell::{builder::Builder, context::Context, module::Module};
use rustpython_parser::ast::Stmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use crate::codegen::generic_codegen::LLVMGenericCodegen;
use crate::codegen::typed_codegen::LLVMTypedCodegen;
use crate::type_inference::Type;

#[derive(Debug)]
pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub sym_table: RefCell<HashMap<String, AnyValueEnum<'ctx>>>,
    pub sym_table_as_any: RefCell<HashMap<String, AnyValueEnum<'ctx>>>,
    pub func_args: RefCell<HashMap<String, AnyValueEnum<'ctx>>>,
    pub any_type: StructType<'ctx>,
    pub any_bool_type: StructType<'ctx>,  // Denoted with 0
    pub any_int_type: StructType<'ctx>,   // Denoted with 1
    pub any_float_type: StructType<'ctx>, // Denoted with 2
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("python_module");
        let i8_type = context.i8_type();

        // TODO: Refactor, there's gotta be a better way to do this.
        let any_type = context.struct_type(&[i8_type.into(), i8_type.array_type(8).into()], false);
        let any_bool_type = context.struct_type(&[i8_type.into(), i8_type.into()], false);
        let any_int_type = context.struct_type(&[i8_type.into(), context.i64_type().into()], false);
        let any_float_type =
            context.struct_type(&[i8_type.into(), context.f64_type().into()], false);

        Self {
            context,
            builder,
            module,
            sym_table: RefCell::new(HashMap::new()),
            sym_table_as_any: RefCell::new(HashMap::new()),
            func_args: RefCell::new(HashMap::new()),
            any_type,
            any_bool_type,
            any_int_type,
            any_float_type,
        }
    }

    pub fn compile(&self, ast: &[Stmt], types: &HashMap<String, Type>) {
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

        // self.dump_module();

        let output = Path::new("outputs/output.ll");

        match self.module.print_to_file(output) {
            Ok(..) => println!(".ll file found at {}", output.display()),
            Err(e) => println!("Could not generate .ll file: {}", e),
        }
    }

    // Realistically, any program compiled without types is
    // is functionally equivalent to a program compiled with types -
    // it's just way slower, and way uglier
    pub fn compile_generically(&self, ast: &[Stmt]) {
        let i32_type = self.context.i32_type();
        self.setup_compiler();

        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");

        self.builder.position_at_end(main_entry);

        for statement in ast {
            match statement.generic_codegen(&self) {
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

    fn setup_compiler(&self) {
        // Declare printf and malloc
        let printf_param_types = BasicMetadataTypeEnum::PointerType(
            self.context.i8_type().ptr_type(AddressSpace::default()),
        );
        let printf_type = self.context.i32_type().fn_type(&[printf_param_types], true);
        let _ = self.module.add_function("printf", printf_type, None);
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let malloc_param_types = BasicMetadataTypeEnum::IntType(
            self.context.i64_type(),
        );
        let malloc_type = i8_ptr_type.fn_type(&[malloc_param_types], false);
        let _ = self.module.add_function("malloc", malloc_type, None);
    }
}
