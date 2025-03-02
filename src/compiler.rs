use inkwell::types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, StructType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum, IntValue};
use inkwell::AddressSpace;
use inkwell::{builder::Builder, context::Context, module::Module};
use rustpython_parser::ast::Stmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::process::{exit, Command};

use crate::codegen::error::{BackendError, IRGenResult};
use crate::codegen::generic_codegen::LLVMGenericCodegen;
use crate::codegen::scope::ScopeManager;
use crate::codegen::typed_codegen::LLVMTypedCodegen;
use crate::type_inference::NodeTypeDB;

#[derive(Debug)]
pub struct Compiler<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub sym_table: ScopeManager<'ctx>,
    pub generically_compile: bool,
    pub func_args: RefCell<Vec<String>>,
    pub any_type: StructType<'ctx>,
    pub object_type: StructType<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, generically_compile: bool) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("pyaotc_mod");

        let any_type = context.opaque_struct_type("struct.HeapObject");
        let object_type = context.opaque_struct_type("struct.Object");
        let _ = context.opaque_struct_type("struct.Iterator");
        let _ = context.opaque_struct_type("struct.Range");
        let _ = context.opaque_struct_type("struct.List");

        Self {
            context,
            builder,
            module,
            sym_table: ScopeManager::new(),
            generically_compile,
            func_args: RefCell::new(Vec::new()),
            any_type,
            object_type,
        }
    }

    pub fn compile(&mut self, ast: &[Stmt], type_db: &NodeTypeDB, file_name: &str) {
        let i32_type = self.context.i32_type();
        self.setup_compiler();

        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");

        self.builder.position_at_end(main_entry);

        // Initialise Boehm GC
        let gc_init = self.module.get_function("GC_init").unwrap();
        let _ = self.builder.build_call(gc_init, &[], "gc_init_call");

        for statement in ast {
            match statement.typed_codegen(self, &type_db) {
                Ok(_ir) => {}
                Err(e) => {
                    eprintln!("{:?}", e);
                    exit(-1);
                }
            }
        }

        // Collect garbage
        let gc_collect = self.module.get_function("GC_gcollect").unwrap();
        let _ = self.builder.build_call(gc_collect, &[], "gc_collect_call");

        let _ = self
            .builder
            .build_return(Some(&i32_type.const_int(0, false)));

        let ll_file_path = format!("outputs/{file_name}.ll");
        let output = Path::new(ll_file_path.as_str());

        match self.module.print_to_file(output) {
            Ok(..) => println!(".ll file found at {}", output.display()),
            Err(e) => println!("Could not generate .ll file: {}", e),
        }

        match self.compile_to_binary(&file_name, &ll_file_path) {
            Ok(..) => {}
            Err(e) => panic!("CompileError: {}", e),
        }
    }

    // Realistically, any program compiled without types is
    // is functionally equivalent to a program compiled with types -
    // it's just way slower, and way uglier
    pub fn compile_generically(&mut self, ast: &[Stmt], file_name: &str) {
        let i32_type = self.context.i32_type();
        self.setup_compiler();

        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");

        self.builder.position_at_end(main_entry);

        // Initialise Boehm GC
        let gc_init = self.module.get_function("GC_init").unwrap();
        let _ = self.builder.build_call(gc_init, &[], "gc_init_call");

        for statement in ast {
            match statement.generic_codegen(self) {
                Ok(_ir) => {}
                Err(e) => {
                    println!("{:?}", e);
                    return;
                }
            }
        }

        // Collect garbage
        let gc_collect = self.module.get_function("GC_gcollect").unwrap();
        let _ = self.builder.build_call(gc_collect, &[], "gc_collect_call");

        let _ = self
            .builder
            .build_return(Some(&i32_type.const_int(0, false)));

        // self.dump_module();

        let ll_file_path = format!("outputs/{file_name}.ll");
        let output = Path::new(&ll_file_path);

        match self.module.print_to_file(output) {
            Ok(..) => println!(".ll file found at {}", output.display()),
            Err(e) => println!("Could not generate .ll file: {}", e),
        }

        match self.compile_to_binary(&file_name, &ll_file_path) {
            Ok(..) => {}
            Err(e) => panic!("CompileError: {}", e),
        }
    }

    // TODO: Refactor error handling to CompileError
    fn compile_to_binary(&self, file_name: &str, ll_file_path: &str) -> Result<(), String> {
        // link generated .ll file to generic LLVM code file
        let build_status = Command::new("./build_prereq_llvm.sh")
            .status()
            .expect("Failed to start build_prereq_llvm.sh");

        if !build_status.success() {
            eprintln!("Error: build_prereq_llvm.sh failed");
            exit(1);
        }

        println!("Successfully ran build_prereq_llvm.sh");

        let prereq_llvm_file = "outputs/prereq_llvm.ll";
        let linked_llvm_file = format!("outputs/{file_name}_final.ll");
        let link_status = Command::new("llvm-link")
            .args([
                &prereq_llvm_file,
                &ll_file_path,
                "-o",
                &linked_llvm_file.as_str(),
            ])
            .status()
            .expect("Failed to start llvm-link");

        if !link_status.success() {
            eprintln!("Error: llvm-link failed");
            exit(1);
        }

        println!("Successfully linked .ll files.");

        let obj_file_path = format!("outputs/{file_name}");
        let llc_status = Command::new("llc")
            .args(["-filetype=obj", &linked_llvm_file, "-o", &obj_file_path])
            .status()
            .map_err(|e| format!("Failed to execute llc: {}", e))?;

        if !llc_status.success() {
            return Err("llc failed to compile LLVM IR to object file".to_string());
        }

        let gcc_status = Command::new("gcc")
            .args([&obj_file_path, "-lgc", "-o", &file_name, "-no-pie"])
            .status()
            .map_err(|e| format!("Failed to execute gcc: {}", e))?;

        if !gcc_status.success() {
            return Err("gcc failed to link object file into final binary".to_string());
        }

        println!(
            "Successfully compiled to binary, try running your program with ./{}!",
            &file_name
        );
        Ok(())
    }

    pub fn dump_module(&self) {
        println!(
            "LLVM IR Representation:\n{}",
            self.module.print_to_string().to_string(),
        );
    }

    /**
     * Declare all functions that the compiler needs.
     */
    fn setup_compiler(&self) {
        self.setup_obj_constructors();
        self.setup_print_fns();
        self.setup_gc_fns();
        self.setup_list_fns();
        self.setup_range_fns();
        self.setup_iter_fns();
    }

    /**
     * Utils to construct Object* versions of primitive types
     */
    fn setup_obj_constructors(&self) {
        let range = self.module.get_struct_type("struct.Range").unwrap();
        let list = self.module.get_struct_type("struct.List").unwrap();
        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let obj_fns = HashMap::from([
            ("new_int", self.context.i64_type().as_any_type_enum()),
            ("new_bool", self.context.bool_type().as_any_type_enum()),
            ("new_float", self.context.f64_type().as_any_type_enum()),
            (
                "new_str",
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .as_any_type_enum(),
            ),
            (
                "new_range",
                range.ptr_type(AddressSpace::default()).as_any_type_enum(),
            ),
            (
                "new_list",
                list.ptr_type(AddressSpace::default()).as_any_type_enum(),
            ),
        ]);
        for (fn_name, fn_param) in obj_fns {
            let param_type = self.convert_any_type_to_param_type(fn_param);
            let params = if let Some(typ) = param_type {
                Vec::from([typ])
            } else {
                Vec::new()
            };
            let obj_constructor_fn = obj_ptr_type.fn_type(&params, false);
            let _ = self.module.add_function(&fn_name, obj_constructor_fn, None);
        }
    }

    fn setup_iter_fns(&self) {
        let iterator = self.module.get_struct_type("struct.Iterator").unwrap();
        let range = self.context.get_struct_type("struct.Range").unwrap();
        let list = self.context.get_struct_type("struct.List").unwrap();

        let iter_fns = Vec::from([
            ("range", range.ptr_type(AddressSpace::default())),
            ("list", list.ptr_type(AddressSpace::default())),
        ]);

        for (fn_name_prefix, fn_param) in iter_fns {
            let fn_type = iterator
                .ptr_type(AddressSpace::default())
                .fn_type(&[fn_param.into()], false);
            let fn_name = format!("{fn_name_prefix}_iter");
            let _ = self.module.add_function(&fn_name, fn_type, None);
        }
    }

    pub fn setup_range_fns(&self) {
        let word_type = self.context.i64_type();
        let range_type = self
            .module
            .get_struct_type("struct.Range")
            .unwrap()
            .ptr_type(AddressSpace::default());

        let range_len_fn_type = word_type.fn_type(&[range_type.into()], false);
        let create_range_fn_type = range_type.fn_type(
            &[word_type.into(), word_type.into(), word_type.into()],
            false,
        );
        let range_index_fn_type = word_type
            .ptr_type(AddressSpace::default())
            .fn_type(&[range_type.into(), word_type.into()], false);

        self.module
            .add_function("range_len", range_len_fn_type, None);
        self.module
            .add_function("create_range", create_range_fn_type, None);
        self.module
            .add_function("range_index", range_index_fn_type, None);
    }

    pub fn setup_list_fns(&self) {
        let void_ptr_type = self
            .context
            .i8_type()
            .ptr_type(inkwell::AddressSpace::default());
        let usize_type = self.context.i64_type();
        let list_type = self
            .module
            .get_struct_type("struct.List")
            .unwrap()
            .ptr_type(AddressSpace::default());
        let elt_type = self.context.i32_type();

        let create_list_fn_type = list_type.fn_type(&[usize_type.into(), elt_type.into()], false);
        let list_len_fn_type = usize_type.fn_type(&[list_type.into()], false);
        let list_index_fn_type =
            void_ptr_type.fn_type(&[list_type.into(), usize_type.into()], false);
        let list_set_fn_type = void_ptr_type.fn_type(
            &[list_type.into(), usize_type.into(), void_ptr_type.into()],
            false,
        );
        let list_append_fn_type =
            list_type.fn_type(&[list_type.into(), void_ptr_type.into()], false);

        self.module
            .add_function("create_list", create_list_fn_type, None);
        self.module.add_function("list_len", list_len_fn_type, None);
        self.module
            .add_function("list_index", list_index_fn_type, None);
        self.module.add_function("list_set", list_set_fn_type, None);
        self.module
            .add_function("list_append", list_append_fn_type, None);
    }

    /**
     * Print utils
     */
    fn setup_print_fns(&self) {
        let range = self.context.get_struct_type("struct.Range").unwrap();
        let list = self.context.get_struct_type("struct.List").unwrap();
        let print_fns = HashMap::from([
            ("print_int", self.context.i64_type().as_any_type_enum()),
            ("print_bool", self.context.bool_type().as_any_type_enum()),
            ("print_float", self.context.f64_type().as_any_type_enum()),
            (
                "print_str",
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .as_any_type_enum(),
            ),
            (
                "print_range",
                range.ptr_type(AddressSpace::default()).as_any_type_enum(),
            ),
            (
                "print_list",
                list.ptr_type(AddressSpace::default()).as_any_type_enum(),
            ),
            (
                "print_obj",
                self.object_type
                    .ptr_type(AddressSpace::default())
                    .as_any_type_enum(),
            ),
            ("print_newline", self.context.void_type().as_any_type_enum()),
            ("print_none", self.context.void_type().as_any_type_enum()),
            (
                "printf",
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .as_any_type_enum(),
            ),
        ]);
        for (fn_name, fn_param) in print_fns {
            let param_type = self.convert_any_type_to_param_type(fn_param);
            let mut params = if let Some(typ) = param_type {
                Vec::from([typ])
            } else {
                Vec::new()
            };
            if fn_name.eq("print_obj") {
                params.insert(0, BasicMetadataTypeEnum::IntType(self.context.i32_type()));
            }
            let is_var_args = fn_name.eq("print_obj") | fn_name.eq("printf");
            let print_fn = if fn_name.eq("printf") {
                self.context.i32_type().fn_type(&params, is_var_args)
            } else {
                self.context.void_type().fn_type(&params, is_var_args)
            };
            let _ = self.module.add_function(&fn_name, print_fn, None);
        }
    }

    pub fn build_gc_malloc_call(&self, size: IntValue<'ctx>) -> IRGenResult<'ctx> {
        let gc_malloc_fn = self.module.get_function("GC_malloc").unwrap();
        match self
            .builder
            .build_call(gc_malloc_fn, &[BasicMetadataValueEnum::IntValue(size)], "")
        {
            Ok(call) => Ok(call.as_any_value_enum()),
            Err(..) => Err(BackendError {
                message: "Could not build GC_malloc.",
            }),
        }
    }

    pub fn convert_any_value_to_param_value<'a>(
        &self,
        value: AnyValueEnum<'a>,
    ) -> Option<BasicMetadataValueEnum<'a>> {
        match value {
            AnyValueEnum::IntValue(int_value) => Some(BasicMetadataValueEnum::IntValue(int_value)),
            AnyValueEnum::FloatValue(float_value) => {
                Some(BasicMetadataValueEnum::FloatValue(float_value))
            }
            AnyValueEnum::PointerValue(pointer_value) => {
                Some(BasicMetadataValueEnum::PointerValue(pointer_value))
            }
            AnyValueEnum::StructValue(struct_value) => {
                Some(BasicMetadataValueEnum::StructValue(struct_value))
            }
            AnyValueEnum::VectorValue(vector_value) => {
                Some(BasicMetadataValueEnum::VectorValue(vector_value))
            }
            AnyValueEnum::ArrayValue(array_value) => {
                Some(BasicMetadataValueEnum::ArrayValue(array_value))
            }
            _ => None,
        }
    }

    pub fn convert_any_type_to_param_type<'a>(
        &self,
        typ: AnyTypeEnum<'a>,
    ) -> Option<BasicMetadataTypeEnum<'a>> {
        match typ {
            AnyTypeEnum::IntType(int_type) => Some(BasicMetadataTypeEnum::IntType(int_type)),
            AnyTypeEnum::FloatType(float_type) => {
                Some(BasicMetadataTypeEnum::FloatType(float_type))
            }
            AnyTypeEnum::PointerType(pointer_type) => {
                Some(BasicMetadataTypeEnum::PointerType(pointer_type))
            }
            AnyTypeEnum::StructType(struct_type) => {
                Some(BasicMetadataTypeEnum::StructType(struct_type))
            }
            AnyTypeEnum::VectorType(vector_type) => {
                Some(BasicMetadataTypeEnum::VectorType(vector_type))
            }
            AnyTypeEnum::ArrayType(array_type) => {
                Some(BasicMetadataTypeEnum::ArrayType(array_type))
            }
            _ => None,
        }
    }

    /**
     * Garbage collection utils...
     * TODO: Add Boehm GC to dependencies...
     */
    fn setup_gc_fns(&self) {
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let i64_type = self.context.i64_type();
        let void_type = self.context.void_type();

        self.module
            .add_function("GC_init", void_type.fn_type(&[], false), None);
        self.module.add_function(
            "GC_malloc",
            i8_ptr_type.fn_type(&[BasicMetadataTypeEnum::IntType(i64_type)], false),
            None,
        );
        self.module.add_function(
            "GC_calloc",
            self.context.i32_type().fn_type(&[], true),
            None,
        );
        self.module.add_function(
            "GC_realloc",
            i8_ptr_type.fn_type(
                &[
                    BasicMetadataTypeEnum::PointerType(i8_ptr_type),
                    BasicMetadataTypeEnum::IntType(i64_type),
                ],
                false,
            ),
            None,
        );
        self.module.add_function(
            "GC_free",
            void_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], false),
            None,
        );
        self.module
            .add_function("GC_gcollect", void_type.fn_type(&[], false), None);
        self.module.add_function(
            "GC_register_finalizer",
            void_type.fn_type(
                &[
                    BasicMetadataTypeEnum::PointerType(i8_ptr_type),
                    BasicMetadataTypeEnum::PointerType(i8_ptr_type),
                    BasicMetadataTypeEnum::PointerType(i8_ptr_type),
                    BasicMetadataTypeEnum::PointerType(i8_ptr_type),
                    BasicMetadataTypeEnum::PointerType(i8_ptr_type),
                ],
                false,
            ),
            None,
        );
        self.module.add_function(
            "GC_set_max_heap_size",
            void_type.fn_type(&[BasicMetadataTypeEnum::IntType(i64_type)], false),
            None,
        );
        self.module.add_function(
            "GC_size",
            i64_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], false),
            None,
        );
        self.module.add_function(
            "GC_base",
            i8_ptr_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], false),
            None,
        );
        self.module.add_function(
            "GC_malloc_uncollectable",
            i8_ptr_type.fn_type(&[BasicMetadataTypeEnum::IntType(i64_type)], false),
            None,
        );
        self.module
            .add_function("GC_get_heap_size", i64_type.fn_type(&[], false), None);
        self.module
            .add_function("GC_get_free_bytes", i64_type.fn_type(&[], false), None);
        self.module
            .add_function("GC_get_bytes_since_gc", i64_type.fn_type(&[], false), None);
        self.module
            .add_function("GC_get_total_bytes", i64_type.fn_type(&[], false), None);
    }
}
