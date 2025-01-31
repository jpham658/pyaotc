use inkwell::types::{BasicMetadataTypeEnum, PointerType, StructType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;
use inkwell::{builder::Builder, context::Context, module::Module};
use rustpython_parser::ast::Stmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use crate::codegen::error::{BackendError, IRGenResult};
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
    
    // TODO: Replace this with tagged pointer, store type at end
    pub any_bool_type: StructType<'ctx>,  // Denoted with 0
    pub any_int_type: StructType<'ctx>,   // Denoted with 1
    pub any_float_type: StructType<'ctx>, // Denoted with 2
    
    pub any_type_info: HashMap<String, TypeInfo>,
    pub object_type: StructType<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("python_module");
        let i8_type = context.i8_type();

        let any_type = context.struct_type(&[i8_type.into(), i8_type.array_type(8).into()], false);
        let object_type = context.opaque_struct_type("Object");
        
        // TODO: Refactor, there's gotta be a better way to do this.
        let any_bool_type = context.struct_type(&[i8_type.into(), i8_type.into()], false);
        let any_int_type = context.struct_type(&[i8_type.into(), context.i64_type().into()], false);
        let any_float_type =
            context.struct_type(&[i8_type.into(), context.f64_type().into()], false);

        let any_type_info = create_type_info_hashmap();
        
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
            any_type_info,
            object_type,
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

        // Initialise Boehm GC
        let gc_init = self.module.get_function("GC_init").unwrap();
        let _ = self.builder.build_call(gc_init, &[], "gc_init_call");

        for statement in ast {
            match statement.typed_codegen(&self, &types) {
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

    /**
     * Declare all functions that the compiler needs.
     */
    fn setup_compiler(&self) {
        // Declare printf and malloc
        let printf_param_types = BasicMetadataTypeEnum::PointerType(
            self.context.i8_type().ptr_type(AddressSpace::default()),
        );
        let printf_type = self.context.i32_type().fn_type(&[printf_param_types], true);
        let _ = self.module.add_function("printf", printf_type, None);
        self.setup_gc_fns();
    }

    /**
     * Garbage collection utils...
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

    fn build_gc_malloc_call(&self, size: i64) -> IRGenResult<'_> {
        let gc_malloc_fn = self.module.get_function("GC_malloc").unwrap();
        match self.builder.build_call(
            gc_malloc_fn,
            &[BasicMetadataValueEnum::IntValue(
                self.context.i64_type().const_int(size as u64, false),
            )],
            "",
        ) {
            Ok(call) => Ok(call.as_any_value_enum()),
            Err(..) => Err(BackendError {
                message: "Could not build GC_malloc.",
            }),
        }
    }

    fn build_object_is_int_fn(&self) {
        let main_entry = self.builder.get_insert_block().unwrap();
        let object_ptr_type = self.object_type.ptr_type(AddressSpace::default());
    }
}

#[derive(Debug)]
struct TypeInfo {
    pub type_num: i8,
    pub tag: u8,
    pub tag_mask: u8,
    pub shift: i64
}

impl TypeInfo {
    pub fn new(type_num: i8, tag: u8, tag_mask: u8, shift: i64) -> Self {
        Self {
            type_num,
            tag,
            tag_mask,
            shift
        }
    }
}

/**
 * Map for all Any type stuff.
 */
fn create_type_info_hashmap() -> HashMap<String, TypeInfo> {
    HashMap::from([
        ("Bool".to_string(), TypeInfo::new(0, 0 as u8, 1 as u8, 1)),
        ("Int".to_string(), TypeInfo::new(1, 1 as u8, 1 as u8, 1)),
        ("Float".to_string(), TypeInfo::new(2, 2 as u8, 2 as u8, 2)),
        ("Str".to_string(), TypeInfo::new(3, 3 as u8, 3 as u8, 2)),
        ("List".to_string(), TypeInfo::new(4, 4 as u8, 4 as u8, 3)),
        ("Set".to_string(), TypeInfo::new(5, 5 as u8, 5 as u8, 3)),
    ])
}
