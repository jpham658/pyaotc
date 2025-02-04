use inkwell::memory_buffer::MemoryBuffer;
use inkwell::types::{BasicMetadataTypeEnum, StructType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicMetadataValueEnum};
use inkwell::AddressSpace;
use inkwell::{builder::Builder, context::Context, module::Module};
use rustpython_parser::ast::Stmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use crate::codegen::error::{BackendError, IRGenResult};
use crate::codegen::generic_codegen::LLVMGenericCodegen;
use crate::codegen::generic_ops::cmp_operators::g_cmpeq::get_eq_fn_ir;
use crate::codegen::typed_codegen::LLVMTypedCodegen;
use crate::type_inference::Type;

// TODO: Move this to separate file
fn get_prereq_llvm_ir() -> String {
    r#"%struct.Object = type opaque
%struct.HeapObject = type { i32, %union.anon }
%union.anon = type { i8* }

@.str = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1
@.str.1 = private unnamed_addr constant [6 x i8] c"True\0A\00", align 1
@.str.2 = private unnamed_addr constant [7 x i8] c"False\0A\00", align 1
@.str.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@.str.4 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@.str.5 = private unnamed_addr constant [23 x i8] c"Not valid heap object.\00", align 1
@.str.6 = private unnamed_addr constant [13 x i8] c"Hello world!\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @object_is_int(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = ptrtoint %struct.Object* %3 to i64
  %5 = and i64 %4, 1
  %6 = icmp eq i64 %5, 1
  ret i1 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i64 @object_as_int(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = ptrtoint %struct.Object* %3 to i64
  %5 = ashr i64 %4, 1
  ret i64 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.Object* @new_int(i64 noundef %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = shl i64 %3, 1
  %5 = or i64 %4, 1
  %6 = inttoptr i64 %5 to %struct.Object*
  ret %struct.Object* %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @object_is_bool(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = ptrtoint %struct.Object* %3 to i64
  %5 = and i64 %4, 3
  %6 = icmp eq i64 %5, 0
  ret i1 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @object_as_bool(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = ptrtoint %struct.Object* %3 to i64
  %5 = lshr i64 %4, 3
  %6 = icmp ne i64 %5, 0
  ret i1 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.Object* @new_bool(i1 noundef %0) #0 {
  %extended = zext i1 %0 to i64
  %2 = alloca i64, align 8
  store i64 %extended, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = shl i64 %3, 3
  %5 = inttoptr i64 %4 to %struct.Object*
  ret %struct.Object* %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @object_is_heap_object(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = ptrtoint %struct.Object* %3 to i64
  %5 = and i64 %4, 2
  %6 = icmp eq i64 %5, 2
  ret i1 %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.HeapObject* @object_address(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = ptrtoint %struct.Object* %3 to i64
  %5 = and i64 %4, -3
  %6 = inttoptr i64 %5 to %struct.HeapObject*
  ret %struct.HeapObject* %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.Object* @object_from_address(%struct.HeapObject* noundef %0) #0 {
  %2 = alloca %struct.HeapObject*, align 8
  store %struct.HeapObject* %0, %struct.HeapObject** %2, align 8
  %3 = load %struct.HeapObject*, %struct.HeapObject** %2, align 8
  %4 = ptrtoint %struct.HeapObject* %3 to i64
  %5 = or i64 %4, 2
  %6 = inttoptr i64 %5 to %struct.Object*
  ret %struct.Object* %6
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @object_type(%struct.Object* noundef %0) #0 {
  %2 = alloca i32, align 4
  %3 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %3, align 8
  %4 = load %struct.Object*, %struct.Object** %3, align 8
  %5 = call zeroext i1 @object_is_int(%struct.Object* noundef %4)
  br i1 %5, label %6, label %7

6:                                                ; preds = %1
  store i32 1, i32* %2, align 4
  br label %16

7:                                                ; preds = %1
  %8 = load %struct.Object*, %struct.Object** %3, align 8
  %9 = call zeroext i1 @object_is_bool(%struct.Object* noundef %8)
  br i1 %9, label %10, label %11

10:                                               ; preds = %7
  store i32 0, i32* %2, align 4
  br label %16

11:                                               ; preds = %7
  %12 = load %struct.Object*, %struct.Object** %3, align 8
  %13 = call %struct.HeapObject* @object_address(%struct.Object* noundef %12)
  %14 = getelementptr inbounds %struct.HeapObject, %struct.HeapObject* %13, i32 0, i32 0
  %15 = load i32, i32* %14, align 8
  store i32 %15, i32* %2, align 4
  br label %16

16:                                               ; preds = %11, %10, %6
  %17 = load i32, i32* %2, align 4
  ret i32 %17
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @object_is_str(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = call i32 @object_type(%struct.Object* noundef %3)
  %5 = icmp eq i32 %4, 2
  ret i1 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local zeroext i1 @object_is_float(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = call i32 @object_type(%struct.Object* noundef %3)
  %5 = icmp eq i32 %4, 3
  ret i1 %5
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i8* @object_as_str(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = call %struct.HeapObject* @object_address(%struct.Object* noundef %3)
  %5 = getelementptr inbounds %struct.HeapObject, %struct.HeapObject* %4, i32 0, i32 1
  %6 = bitcast %union.anon* %5 to i8**
  %7 = load i8*, i8** %6, align 8
  ret i8* %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local double @object_as_float(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = call %struct.HeapObject* @object_address(%struct.Object* noundef %3)
  %5 = getelementptr inbounds %struct.HeapObject, %struct.HeapObject* %4, i32 0, i32 1
  %6 = bitcast %union.anon* %5 to double*
  %7 = load double, double* %6, align 8
  ret double %7
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.Object* @new_str(i8* noundef %0) #0 {
  %2 = alloca i8*, align 8
  %3 = alloca %struct.HeapObject*, align 8
  %4 = alloca %struct.HeapObject, align 8
  store i8* %0, i8** %2, align 8
  %5 = call i8* @malloc(i64 noundef 16)
  %6 = bitcast i8* %5 to %struct.HeapObject*
  store %struct.HeapObject* %6, %struct.HeapObject** %3, align 8
  %7 = load %struct.HeapObject*, %struct.HeapObject** %3, align 8
  %8 = getelementptr inbounds %struct.HeapObject, %struct.HeapObject* %4, i32 0, i32 0
  store i32 2, i32* %8, align 8
  %9 = getelementptr inbounds %struct.HeapObject, %struct.HeapObject* %4, i32 0, i32 1
  %10 = bitcast %union.anon* %9 to i8**
  %11 = load i8*, i8** %2, align 8
  store i8* %11, i8** %10, align 8
  %12 = bitcast %struct.HeapObject* %7 to i8*
  %13 = bitcast %struct.HeapObject* %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %12, i8* align 8 %13, i64 16, i1 false)
  %14 = load %struct.HeapObject*, %struct.HeapObject** %3, align 8
  %15 = call %struct.Object* @object_from_address(%struct.HeapObject* noundef %14)
  ret %struct.Object* %15
}

declare i8* @malloc(i64 noundef) #1

; Function Attrs: argmemonly nofree nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.Object* @new_float(double noundef %0) #0 {
  %2 = alloca double, align 8
  %3 = alloca %struct.HeapObject*, align 8
  %4 = alloca %struct.HeapObject, align 8
  store double %0, double* %2, align 8
  %5 = call i8* @malloc(i64 noundef 16)
  %6 = bitcast i8* %5 to %struct.HeapObject*
  store %struct.HeapObject* %6, %struct.HeapObject** %3, align 8
  %7 = load %struct.HeapObject*, %struct.HeapObject** %3, align 8
  %8 = getelementptr inbounds %struct.HeapObject, %struct.HeapObject* %4, i32 0, i32 0
  store i32 3, i32* %8, align 8
  %9 = getelementptr inbounds %struct.HeapObject, %struct.HeapObject* %4, i32 0, i32 1
  %10 = bitcast %union.anon* %9 to double*
  %11 = load double, double* %2, align 8
  store double %11, double* %10, align 8
  %12 = bitcast %struct.HeapObject* %7 to i8*
  %13 = bitcast %struct.HeapObject* %4 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %12, i8* align 8 %13, i64 16, i1 false)
  %14 = load %struct.HeapObject*, %struct.HeapObject** %3, align 8
  %15 = call %struct.Object* @object_from_address(%struct.HeapObject* noundef %14)
  ret %struct.Object* %15
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_int(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([5 x i8], [5 x i8]* @.str, i64 0, i64 0), i32 noundef %3)
  ret void
}

declare i32 @printf(i8* noundef, ...) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_bool(i1 noundef zeroext %0) #0 {
  %2 = alloca i8, align 1
  %3 = alloca i8*, align 8
  %4 = zext i1 %0 to i8
  store i8 %4, i8* %2, align 1
  %5 = load i8, i8* %2, align 1
  %6 = trunc i8 %5 to i1
  %7 = zext i1 %6 to i64
  %8 = select i1 %6, i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.2, i64 0, i64 0)
  store i8* %8, i8** %3, align 8
  %9 = load i8*, i8** %3, align 8
  %10 = call i32 (i8*, ...) @printf(i8* noundef %9)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_str(i8* noundef %0) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.3, i64 0, i64 0), i8* noundef %3)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_float(double noundef %0) #0 {
  %2 = alloca double, align 8
  store double %0, double* %2, align 8
  %3 = load double, double* %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i64 0, i64 0), double noundef %3)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_obj(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = call i32 @object_type(%struct.Object* noundef %3)
  switch i32 %4, label %15 [
    i32 1, label %5
    i32 0, label %9
    i32 2, label %12
  ]

5:                                                ; preds = %1
  %6 = load %struct.Object*, %struct.Object** %2, align 8
  %7 = call i64 @object_as_int(%struct.Object* noundef %6)
  %8 = trunc i64 %7 to i32
  call void @print_int(i32 noundef %8)
  br label %18

9:                                                ; preds = %1
  %10 = load %struct.Object*, %struct.Object** %2, align 8
  %11 = call zeroext i1 @object_as_bool(%struct.Object* noundef %10)
  call void @print_bool(i1 noundef zeroext %11)
  br label %18

12:                                               ; preds = %1
  %13 = load %struct.Object*, %struct.Object** %2, align 8
  %14 = call i8* @object_as_str(%struct.Object* noundef %13)
  call void @print_str(i8* noundef %14)
  br label %18

15:                                               ; preds = %1
  %16 = load %struct.Object*, %struct.Object** %2, align 8
  %17 = call double @object_as_float(%struct.Object* noundef %16)
  call void @print_float(double noundef %17)
  br label %18

18:                                               ; preds = %15, %12, %9, %5
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_heap_obj(%struct.HeapObject* noundef %0) #0 {
  %2 = alloca %struct.HeapObject*, align 8
  store %struct.HeapObject* %0, %struct.HeapObject** %2, align 8
  %3 = load %struct.HeapObject*, %struct.HeapObject** %2, align 8
  %4 = bitcast %struct.HeapObject* %3 to %struct.Object*
  %5 = call zeroext i1 @object_is_str(%struct.Object* noundef %4)
  br i1 %5, label %6, label %10

6:                                                ; preds = %1
  %7 = load %struct.HeapObject*, %struct.HeapObject** %2, align 8
  %8 = bitcast %struct.HeapObject* %7 to %struct.Object*
  %9 = call i8* @object_as_str(%struct.Object* noundef %8)
  call void @print_str(i8* noundef %9)
  br label %21

10:                                               ; preds = %1
  %11 = load %struct.HeapObject*, %struct.HeapObject** %2, align 8
  %12 = bitcast %struct.HeapObject* %11 to %struct.Object*
  %13 = call zeroext i1 @object_is_float(%struct.Object* noundef %12)
  br i1 %13, label %14, label %18

14:                                               ; preds = %10
  %15 = load %struct.HeapObject*, %struct.HeapObject** %2, align 8
  %16 = bitcast %struct.HeapObject* %15 to %struct.Object*
  %17 = call double @object_as_float(%struct.Object* noundef %16)
  call void @print_float(double noundef %17)
  br label %20

18:                                               ; preds = %10
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.str.5, i64 0, i64 0))
  br label %20

20:                                               ; preds = %18, %14
  br label %21

21:                                               ; preds = %20, %6
  ret void
}
"#.to_string() + &get_eq_fn_ir().to_string()
}
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
        // TODO: Make this work if you can?
        // Otherwise, just keep going with the builder stuff.
        //
        let prereq_llvm_ir = get_prereq_llvm_ir();
        let mem_buffer = MemoryBuffer::create_from_memory_range_copy(prereq_llvm_ir.as_bytes(), "");
        let module = context
            .create_module_from_ir(mem_buffer)
            .expect("Could not create module from LLVM IR.");

        let i8_type = context.i8_type();

        // let union_any_val_type =
        //     context.struct_type(&[i8_type.ptr_type(AddressSpace::default()).into()], false);
        // // TODO: Rename to any_obj when refactoring code
        // let any_type = context.struct_type(&[i8_type.into(), union_any_val_type.into()], false);

        let union_any_val_type = context.get_struct_type("union.anon").unwrap();
        let any_type = context.get_struct_type("struct.HeapObject").unwrap();
        let object_type = context.get_struct_type("struct.Object").unwrap(); // Should not fail if context is set up properly.

        // TODO: Refactor, there's gotta be a better way to do this.
        // TODO: Remove this when we fully implement tagged pointers approach with heap objects.
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

        // self.setup_tagged_ptr_fns();

        // Initialise Boehm GC
        // let gc_init = self.module.get_function("GC_init").unwrap();
        // let _ = self.builder.build_call(gc_init, &[], "gc_init_call");

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
        // let gc_collect = self.module.get_function("GC_gcollect").unwrap();
        // let _ = self.builder.build_call(gc_collect, &[], "gc_collect_call");

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

        // self.setup_tagged_ptr_fns();

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
        // self.setup_gc_fns();
    }

    /**
     * Garbage collection utils...
     * TODO: Add Boehm GC to dependencies...
     */
    // fn setup_gc_fns(&self) {
    //     let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
    //     let i64_type = self.context.i64_type();
    //     let void_type = self.context.void_type();

    //     self.module
    //         .add_function("GC_init", void_type.fn_type(&[], false), None);
    //     self.module.add_function(
    //         "GC_malloc",
    //         i8_ptr_type.fn_type(&[BasicMetadataTypeEnum::IntType(i64_type)], false),
    //         None,
    //     );
    //     self.module.add_function(
    //         "GC_calloc",
    //         self.context.i32_type().fn_type(&[], true),
    //         None,
    //     );
    //     self.module.add_function(
    //         "GC_realloc",
    //         i8_ptr_type.fn_type(
    //             &[
    //                 BasicMetadataTypeEnum::PointerType(i8_ptr_type),
    //                 BasicMetadataTypeEnum::IntType(i64_type),
    //             ],
    //             false,
    //         ),
    //         None,
    //     );
    //     self.module.add_function(
    //         "GC_free",
    //         void_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], false),
    //         None,
    //     );
    //     self.module
    //         .add_function("GC_gcollect", void_type.fn_type(&[], false), None);
    //     self.module.add_function(
    //         "GC_register_finalizer",
    //         void_type.fn_type(
    //             &[
    //                 BasicMetadataTypeEnum::PointerType(i8_ptr_type),
    //                 BasicMetadataTypeEnum::PointerType(i8_ptr_type),
    //                 BasicMetadataTypeEnum::PointerType(i8_ptr_type),
    //                 BasicMetadataTypeEnum::PointerType(i8_ptr_type),
    //                 BasicMetadataTypeEnum::PointerType(i8_ptr_type),
    //             ],
    //             false,
    //         ),
    //         None,
    //     );
    //     self.module.add_function(
    //         "GC_set_max_heap_size",
    //         void_type.fn_type(&[BasicMetadataTypeEnum::IntType(i64_type)], false),
    //         None,
    //     );
    //     self.module.add_function(
    //         "GC_size",
    //         i64_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], false),
    //         None,
    //     );
    //     self.module.add_function(
    //         "GC_base",
    //         i8_ptr_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], false),
    //         None,
    //     );
    //     self.module.add_function(
    //         "GC_malloc_uncollectable",
    //         i8_ptr_type.fn_type(&[BasicMetadataTypeEnum::IntType(i64_type)], false),
    //         None,
    //     );
    //     self.module
    //         .add_function("GC_get_heap_size", i64_type.fn_type(&[], false), None);
    //     self.module
    //         .add_function("GC_get_free_bytes", i64_type.fn_type(&[], false), None);
    //     self.module
    //         .add_function("GC_get_bytes_since_gc", i64_type.fn_type(&[], false), None);
    //     self.module
    //         .add_function("GC_get_total_bytes", i64_type.fn_type(&[], false), None);
    // }

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

    fn setup_tagged_ptr_fns(&self) {
        // Set up for bool tagged ptrs - tagged 0
        let _ = self.build_object_is_bool_fn();
        let _ = self.build_object_as_bool_fn();
        let _ = self.build_new_bool_fn();

        // Set up for int tagged ptrs - tagged 1
        let _ = self.build_object_is_int_fn();
        let _ = self.build_object_as_int_fn();
        let _ = self.build_new_int_fn();

        // Set up for heap object tagged ptrs - tagged 2
        let _ = self.build_object_is_heap_obj_fn();
        let _ = self.build_object_address_fn();
        let _ = self.build_object_from_address_fn();

        // Set up for generic object type testers
        let _ = self.build_object_type_fn();

        // Set up for string (heap object)
        let _ = self.build_object_is_str_fn();
        let _ = self.build_object_as_str_fn();
        let _ = self.build_new_str_fn();

        // Set up for float (heap object)
        let _ = self.build_object_is_float_fn();
        let _ = self.build_object_as_float_fn();
        let _ = self.build_new_float_fn();

        // TODO: Extend for list, set types
    }

    fn build_object_is_bool_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let params = &[BasicMetadataTypeEnum::PointerType(obj_ptr_type)];

        let bool_type = self.context.bool_type();
        let object_is_bool_fn_type = bool_type.fn_type(params, false);
        let object_is_bool_fn =
            self.module
                .add_function("object_is_bool", object_is_bool_fn_type, None);

        let entry_block = self.context.append_basic_block(object_is_bool_fn, "entry");
        self.builder.position_at_end(entry_block);

        let object_ptr = object_is_bool_fn.get_nth_param(0).unwrap();
        let ptr_to_obj_ptr = self
            .builder
            .build_alloca(obj_ptr_type, "")
            .expect("Could not allocate memory.");
        let _ = self.builder.build_store(ptr_to_obj_ptr, object_ptr);
        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_obj_ptr, "")
            .expect("Could not load pointer.");
        let obj_ptr_as_int = self
            .builder
            .build_ptr_to_int(
                copied_obj_ptr.into_pointer_value(),
                self.context.i64_type(),
                "",
            )
            .expect("Could not convert pointer to integer.");
        let tag_mask = self.any_type_info["Bool"].tag_mask;
        let llvm_tag_mask = self.context.i64_type().const_int(tag_mask.into(), false);
        let obj_ptr_tag = self
            .builder
            .build_and(obj_ptr_as_int, llvm_tag_mask, "")
            .expect("Could not perform 'and'.");
        let tag = self.any_type_info["Bool"].tag;
        let llvm_tag = self.context.i64_type().const_int(tag as u64, false);
        let is_bool = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, obj_ptr_tag, llvm_tag, "")
            .expect("Could not compare int values.");

        let _ = self.builder.build_return(Some(&is_bool));

        self.builder.position_at_end(main_entry);

        Ok(object_is_bool_fn.as_any_value_enum())
    }

    fn build_object_as_bool_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let bool_type = self.context.bool_type();
        let i64_type = self.context.i64_type();
        let object_ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let fn_type = bool_type.fn_type(&[object_ptr_type.into()], false);
        let object_as_bool_fn = self.module.add_function("object_as_bool", fn_type, None);

        let entry = self.context.append_basic_block(object_as_bool_fn, "entry");

        let _ = self.builder.position_at_end(entry);

        let ptr_to_object_ptr = self
            .builder
            .build_alloca(object_ptr_type, "")
            .expect("Could not allocate memory.");

        let object_ptr = object_as_bool_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();

        let _ = self.builder.build_store(ptr_to_object_ptr, object_ptr);

        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_object_ptr, "")
            .expect("Could not load value.");

        let ptr_to_int = self
            .builder
            .build_ptr_to_int(copied_obj_ptr.into_pointer_value(), i64_type, "ptr_to_int")
            .expect("Could not convert pointer to int.");

        let shift_bits = self.any_type_info["Bool"].shift;
        let llvm_shift_bits = i64_type.const_int(shift_bits as u64, false);

        let shifted_value = self
            .builder
            .build_right_shift(ptr_to_int, llvm_shift_bits, false, "")
            .expect("Could not shift ptr right.");

        let shifted_value_as_i1 = self
            .builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                shifted_value,
                i64_type.const_int(0 as u64, false),
                "",
            )
            .expect("Could not compare bool.");

        let _ = self.builder.build_return(Some(&shifted_value_as_i1));

        self.builder.position_at_end(main_entry);

        Ok(object_as_bool_fn.as_any_value_enum())
    }

    fn build_new_bool_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i64_type = self.context.i64_type();
        let bool_type = self.context.bool_type();
        let ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let fn_type = ptr_type.fn_type(&[bool_type.into()], false);
        let new_bool_fn = self.module.add_function("new_bool", fn_type, None);

        let entry = self.context.append_basic_block(new_bool_fn, "entry");
        self.builder.position_at_end(entry);

        let ptr_to_int = self
            .builder
            .build_alloca(i64_type, "")
            .expect("Could not allocate memory.");

        let param = new_bool_fn.get_nth_param(0).unwrap().into_int_value();
        let param_as_i64 = self
            .builder
            .build_int_cast(param, i64_type, "")
            .expect("Could not convert bool to int.");
        let _ = self.builder.build_store(ptr_to_int, param_as_i64);

        let int_val = self
            .builder
            .build_load(ptr_to_int, "loaded")
            .expect("Could not load value.")
            .into_int_value();

        let shift_bits = self.any_type_info["Bool"].shift;
        let llvm_shift_bits = i64_type.const_int(shift_bits as u64, false);
        let shifted_value = self
            .builder
            .build_left_shift(int_val, llvm_shift_bits, "")
            .expect("Could not shift int value to left.");
        let tag = self.any_type_info["Bool"].tag;
        let llvm_tag = i64_type.const_int(tag as u64, false);
        let tagged_value = self
            .builder
            .build_or(shifted_value, llvm_tag, "")
            .expect("Could not tag int value.");

        let tagged_ptr = self
            .builder
            .build_int_to_ptr(tagged_value, ptr_type, "")
            .expect("Could not convert int to pointer.");

        let _ = self.builder.build_return(Some(&tagged_ptr));

        let _ = self.builder.position_at_end(main_entry);

        Ok(new_bool_fn.as_any_value_enum())
    }

    fn build_object_is_int_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let params = &[BasicMetadataTypeEnum::PointerType(obj_ptr_type)];

        let bool_type = self.context.bool_type();
        let object_is_int_fn_type = bool_type.fn_type(params, false);
        let object_is_int_fn =
            self.module
                .add_function("object_is_int", object_is_int_fn_type, None);

        let entry_block = self.context.append_basic_block(object_is_int_fn, "entry");
        self.builder.position_at_end(entry_block);

        let object_ptr = object_is_int_fn.get_nth_param(0).unwrap();
        let ptr_to_obj_ptr = self
            .builder
            .build_alloca(obj_ptr_type, "")
            .expect("Could not allocate memory.");
        let _ = self.builder.build_store(ptr_to_obj_ptr, object_ptr);
        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_obj_ptr, "")
            .expect("Could not load pointer.");
        let obj_ptr_as_int = self
            .builder
            .build_ptr_to_int(
                copied_obj_ptr.into_pointer_value(),
                self.context.i64_type(),
                "",
            )
            .expect("Could not convert pointer to integer.");
        let tag_mask = self.any_type_info["Int"].tag_mask;
        let llvm_tag_mask = self.context.i64_type().const_int(tag_mask.into(), false);
        let obj_ptr_tag = self
            .builder
            .build_and(obj_ptr_as_int, llvm_tag_mask, "")
            .expect("Could not perform 'and'.");
        let tag = self.any_type_info["Int"].tag;
        let llvm_tag = self.context.i64_type().const_int(tag as u64, false);
        let is_int = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, obj_ptr_tag, llvm_tag, "")
            .expect("Could not compare int values.");
        let _ = self.builder.build_return(Some(&is_int));

        self.builder.position_at_end(main_entry);

        Ok(object_is_int_fn.as_any_value_enum())
    }

    fn build_object_as_int_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i64_type = self.context.i64_type();
        let object_ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let fn_type = i64_type.fn_type(&[object_ptr_type.into()], false);
        let object_as_int_fn = self.module.add_function("object_as_int", fn_type, None);

        let entry = self.context.append_basic_block(object_as_int_fn, "entry");

        let _ = self.builder.position_at_end(entry);

        let ptr_to_object_ptr = self
            .builder
            .build_alloca(object_ptr_type, "")
            .expect("Could not allocate memory.");

        let object_ptr = object_as_int_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();

        let _ = self.builder.build_store(ptr_to_object_ptr, object_ptr);

        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_object_ptr, "")
            .expect("Could not load value.");

        let ptr_to_int = self
            .builder
            .build_ptr_to_int(copied_obj_ptr.into_pointer_value(), i64_type, "ptr_to_int")
            .expect("Could not convert pointer to int.");

        let shift_bits = self.any_type_info["Int"].shift;
        let llvm_shift_bits = i64_type.const_int(shift_bits as u64, false);

        let shifted_value = self
            .builder
            .build_right_shift(ptr_to_int, llvm_shift_bits, false, "")
            .expect("Could not shift ptr right.");

        let _ = self.builder.build_return(Some(&shifted_value));

        self.builder.position_at_end(main_entry);

        Ok(object_as_int_fn.as_any_value_enum())
    }

    fn build_new_int_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i64_type = self.context.i64_type();
        let ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
        let new_int_fn = self.module.add_function("new_int", fn_type, None);

        let entry = self.context.append_basic_block(new_int_fn, "entry");
        self.builder.position_at_end(entry);

        let ptr_to_int = self
            .builder
            .build_alloca(i64_type, "")
            .expect("Could not allocate memory.");

        let param = new_int_fn.get_nth_param(0).unwrap().into_int_value();
        let _ = self.builder.build_store(ptr_to_int, param);

        let int_val = self
            .builder
            .build_load(ptr_to_int, "loaded")
            .expect("Could not load value.")
            .into_int_value();

        let shift_bits = self.any_type_info["Int"].shift;
        let llvm_shift_bits = i64_type.const_int(shift_bits as u64, false);
        let shifted_value = self
            .builder
            .build_left_shift(int_val, llvm_shift_bits, "")
            .expect("Could not shift int value to left.");
        let tag = self.any_type_info["Int"].tag;
        let llvm_tag = i64_type.const_int(tag as u64, false);
        let tagged_value = self
            .builder
            .build_or(shifted_value, llvm_tag, "")
            .expect("Could not tag int value.");

        let tagged_ptr = self
            .builder
            .build_int_to_ptr(tagged_value, ptr_type, "")
            .expect("Could not convert int to pointer.");

        let _ = self.builder.build_return(Some(&tagged_ptr));

        let _ = self.builder.position_at_end(main_entry);

        Ok(new_int_fn.as_any_value_enum())
    }

    fn build_object_is_heap_obj_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let params = &[BasicMetadataTypeEnum::PointerType(obj_ptr_type)];

        let bool_type = self.context.bool_type();
        let fn_type = bool_type.fn_type(params, false);
        let object_is_heap_obj_fn = self
            .module
            .add_function("object_is_heap_obj", fn_type, None);

        let entry_block = self
            .context
            .append_basic_block(object_is_heap_obj_fn, "entry");
        self.builder.position_at_end(entry_block);

        let object_ptr = object_is_heap_obj_fn.get_nth_param(0).unwrap();
        let ptr_to_obj_ptr = self
            .builder
            .build_alloca(obj_ptr_type, "")
            .expect("Could not allocate memory.");
        let _ = self.builder.build_store(ptr_to_obj_ptr, object_ptr);
        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_obj_ptr, "")
            .expect("Could not load pointer.");
        let obj_ptr_as_int = self
            .builder
            .build_ptr_to_int(
                copied_obj_ptr.into_pointer_value(),
                self.context.i64_type(),
                "",
            )
            .expect("Could not convert pointer to integer.");
        let tag_mask = self.any_type_info["Heap"].tag_mask;
        let llvm_tag_mask = self.context.i64_type().const_int(tag_mask.into(), false);
        let obj_ptr_tag = self
            .builder
            .build_and(obj_ptr_as_int, llvm_tag_mask, "")
            .expect("Could not perform 'and'.");
        let tag = self.any_type_info["Heap"].tag;
        let llvm_tag = self.context.i64_type().const_int(tag as u64, false);
        let is_heap_obj = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, obj_ptr_tag, llvm_tag, "")
            .expect("Could not compare int values.");
        let _ = self.builder.build_return(Some(&is_heap_obj));

        self.builder.position_at_end(main_entry);

        Ok(object_is_heap_obj_fn.as_any_value_enum())
    }

    fn build_object_address_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i64_type = self.context.i64_type();
        let any_obj_ptr_type = self.any_type.ptr_type(AddressSpace::default());
        let object_ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let fn_type = any_obj_ptr_type.fn_type(&[object_ptr_type.into()], false);
        let object_address_fn = self.module.add_function("object_address", fn_type, None);

        let entry = self.context.append_basic_block(object_address_fn, "entry");

        let _ = self.builder.position_at_end(entry);

        let ptr_to_object_ptr = self
            .builder
            .build_alloca(object_ptr_type, "")
            .expect("Could not allocate memory.");

        let object_ptr = object_address_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();

        let _ = self.builder.build_store(ptr_to_object_ptr, object_ptr);

        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_object_ptr, "")
            .expect("Could not load value.");

        let ptr_to_int = self
            .builder
            .build_ptr_to_int(copied_obj_ptr.into_pointer_value(), i64_type, "ptr_to_int")
            .expect("Could not convert pointer to int.");

        let tag_mask = !(self.any_type_info["Heap"].tag_mask as u64);
        let llvm_tag_mask = i64_type.const_int(tag_mask, false);
        let address_as_int = self
            .builder
            .build_and(ptr_to_int, llvm_tag_mask, "")
            .expect("Could not build 'and'.");
        let address_as_ptr = self
            .builder
            .build_int_to_ptr(address_as_int, any_obj_ptr_type, "")
            .expect("Could not convert int to AnyType pointer.");
        let _ = self.builder.build_return(Some(&address_as_ptr));

        self.builder.position_at_end(main_entry);

        Ok(object_address_fn.as_any_value_enum())
    }

    fn build_object_from_address_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i64_type = self.context.i64_type();
        let object_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let any_object_ptr_type = self.any_type.ptr_type(AddressSpace::default());

        let fn_type = object_ptr_type.fn_type(&[any_object_ptr_type.into()], false);
        let object_from_address_fn = self
            .module
            .add_function("object_from_address", fn_type, None);

        let entry = self
            .context
            .append_basic_block(object_from_address_fn, "entry");

        let _ = self.builder.position_at_end(entry);

        let ptr_to_object_ptr = self
            .builder
            .build_alloca(any_object_ptr_type, "")
            .expect("Could not allocate memory.");

        let object_ptr = object_from_address_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();

        let _ = self.builder.build_store(ptr_to_object_ptr, object_ptr);

        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_object_ptr, "")
            .expect("Could not load value.");

        let ptr_to_int = self
            .builder
            .build_ptr_to_int(copied_obj_ptr.into_pointer_value(), i64_type, "ptr_to_int")
            .expect("Could not convert pointer to int.");

        let tag = self.any_type_info["Heap"].tag as u64;
        let llvm_tag = i64_type.const_int(tag, false);
        let heap_obj_as_int = self
            .builder
            .build_or(ptr_to_int, llvm_tag, "")
            .expect("Could not build 'or'.");
        let heap_obj_as_ptr = self
            .builder
            .build_int_to_ptr(heap_obj_as_int, object_ptr_type, "")
            .expect("Could not convert int to Object pointer.");
        let _ = self.builder.build_return(Some(&heap_obj_as_ptr));

        self.builder.position_at_end(main_entry);

        Ok(object_from_address_fn.as_any_value_enum())
    }

    fn build_object_type_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i8_type = self.context.i8_type();
        let object_ptr_type = self.object_type.ptr_type(AddressSpace::default());

        let fn_type = i8_type.fn_type(&[object_ptr_type.into()], false);
        let object_type_fn = self.module.add_function("object_type", fn_type, None);

        let entry = self.context.append_basic_block(object_type_fn, "entry");
        let _ = self.builder.position_at_end(entry);

        let ptr_to_object_ptr = self
            .builder
            .build_alloca(object_ptr_type, "")
            .expect("Could not allocate memory.");
        let type_ptr = self
            .builder
            .build_alloca(i8_type, "")
            .expect("Could not allocate memory.");

        let object_ptr = object_type_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();

        let _ = self.builder.build_store(ptr_to_object_ptr, object_ptr);

        let copied_obj_ptr = self
            .builder
            .build_load(ptr_to_object_ptr, "")
            .expect("Could not load value.");

        // Get type tester functions
        let object_is_heap_obj_fn = self.module.get_function("object_is_heap_obj").unwrap();
        let object_is_int_fn = self.module.get_function("object_is_int").unwrap();
        let object_is_bool_fn = self.module.get_function("object_is_bool").unwrap();

        // Create different condition blocks
        let handle_heap_obj = self
            .context
            .append_basic_block(object_type_fn, "handle_heap_obj");
        let test_int_obj = self
            .context
            .append_basic_block(object_type_fn, "test_int_obj");
        let handle_int_obj = self
            .context
            .append_basic_block(object_type_fn, "handle_int_obj");
        let test_bool_obj = self
            .context
            .append_basic_block(object_type_fn, "test_bool_obj");
        let handle_bool_obj = self
            .context
            .append_basic_block(object_type_fn, "handle_bool_obj");
        let handle_invalid_obj = self
            .context
            .append_basic_block(object_type_fn, "handle_invalid_obj");
        let merge_block = self.context.append_basic_block(object_type_fn, "merge");

        let is_heap_object = self
            .builder
            .build_call(object_is_heap_obj_fn, &[copied_obj_ptr.into()], "")
            .expect("")
            .as_any_value_enum();
        let _ = self.builder.build_conditional_branch(
            is_heap_object.into_int_value(),
            handle_heap_obj,
            test_int_obj,
        );

        // TEST BLOCKS
        // test_int_obj
        let _ = self.builder.position_at_end(test_int_obj);
        let is_int_object = self
            .builder
            .build_call(object_is_int_fn, &[copied_obj_ptr.into()], "")
            .expect("")
            .as_any_value_enum();
        let _ = self.builder.build_conditional_branch(
            is_int_object.into_int_value(),
            handle_int_obj,
            test_bool_obj,
        );

        // test_bool_obj
        let _ = self.builder.position_at_end(test_bool_obj);
        let is_bool_object = self
            .builder
            .build_call(object_is_bool_fn, &[copied_obj_ptr.into()], "")
            .expect("")
            .as_any_value_enum();
        let _ = self.builder.build_conditional_branch(
            is_bool_object.into_int_value(),
            handle_bool_obj,
            handle_invalid_obj,
        );

        // HANDLE BLOCKS
        // handle_heap_obj
        let _ = self.builder.position_at_end(handle_heap_obj);
        let obj_ptr = self
            .builder
            .build_load(ptr_to_object_ptr, "")
            .expect("Could not load from pointer.");
        let obj_address_fn = self.module.get_function("object_address").unwrap();
        let any_obj_ptr = self
            .builder
            .build_call(obj_address_fn, &[obj_ptr.into()], "")
            .expect("Could not get object address.")
            .as_any_value_enum();
        let ptr_to_type = self
            .builder
            .build_struct_gep(any_obj_ptr.into_pointer_value(), 0, "")
            .expect("Could not get pointer to Any object type.");
        let heap_obj_type = self
            .builder
            .build_load(ptr_to_type, "")
            .expect("Could not load value.");
        let _ = self.builder.build_store(type_ptr, heap_obj_type);
        let _ = self.builder.build_unconditional_branch(merge_block);

        // handle_int_obj
        let _ = self.builder.position_at_end(handle_int_obj);
        let int_type = i8_type.const_int(1, false);
        let _ = self.builder.build_store(type_ptr, int_type);
        let _ = self.builder.build_unconditional_branch(merge_block);

        // handle_bool_obj
        let _ = self.builder.position_at_end(handle_bool_obj);
        let int_type = i8_type.const_int(0, false);
        let _ = self.builder.build_store(type_ptr, int_type);
        let _ = self.builder.build_unconditional_branch(merge_block);

        // handle_invalid_obj
        let _ = self.builder.position_at_end(handle_invalid_obj);
        let _ = self.builder.build_unreachable();

        // merge_block
        let _ = self.builder.position_at_end(merge_block);
        let type_val = self
            .builder
            .build_load(type_ptr, "")
            .expect("Could not load value.");
        let _ = self.builder.build_return(Some(&type_val));

        self.builder.position_at_end(main_entry);

        Ok(object_type_fn.as_any_value_enum())
    }

    fn build_object_is_str_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let bool_type = self.context.bool_type();
        let i8_type = self.context.i8_type();
        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let fn_type = bool_type.fn_type(&[obj_ptr_type.into()], false);
        let object_is_str_fn = self.module.add_function("object_is_str", fn_type, None);
        let entry = self.context.append_basic_block(object_is_str_fn, "entry");
        let _ = self.builder.position_at_end(entry);

        let param = object_is_str_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();
        let ptr_to_obj_ptr = self
            .builder
            .build_alloca(obj_ptr_type, "")
            .expect("Could not build alloc.");
        let _ = self.builder.build_store(ptr_to_obj_ptr, param);
        let obj_ptr = self
            .builder
            .build_load(ptr_to_obj_ptr, "")
            .expect("Could not load pointer.");
        let obj_type_fn = self.module.get_function("object_type").unwrap();
        let obj_type = self
            .builder
            .build_call(obj_type_fn, &[obj_ptr.into()], "")
            .expect("Could not get object type.")
            .as_any_value_enum()
            .into_int_value();
        let str_type = i8_type.const_int(2, false);
        let is_str = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, obj_type, str_type, "")
            .expect("Could not compare types.");
        let _ = self.builder.build_return(Some(&is_str));

        let _ = self.builder.position_at_end(main_entry);
        Ok(object_is_str_fn.as_any_value_enum())
    }

    fn build_object_is_float_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let bool_type = self.context.bool_type();
        let i8_type = self.context.i8_type();
        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let fn_type = bool_type.fn_type(&[obj_ptr_type.into()], false);
        let object_is_float_fn = self.module.add_function("object_is_float", fn_type, None);
        let entry = self.context.append_basic_block(object_is_float_fn, "entry");
        let _ = self.builder.position_at_end(entry);

        let param = object_is_float_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();
        let ptr_to_obj_ptr = self
            .builder
            .build_alloca(obj_ptr_type, "")
            .expect("Could not build alloc.");
        let _ = self.builder.build_store(ptr_to_obj_ptr, param);
        let obj_ptr = self
            .builder
            .build_load(ptr_to_obj_ptr, "")
            .expect("Could not load pointer.");
        let obj_type_fn = self.module.get_function("object_type").unwrap();
        let obj_type = self
            .builder
            .build_call(obj_type_fn, &[obj_ptr.into()], "")
            .expect("Could not get object type.")
            .as_any_value_enum()
            .into_int_value();
        let float_type = i8_type.const_int(3, false);
        let is_float = self
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, obj_type, float_type, "")
            .expect("Could not compare types.");
        let _ = self.builder.build_return(Some(&is_float));

        let _ = self.builder.position_at_end(main_entry);
        Ok(object_is_float_fn.as_any_value_enum())
    }

    fn build_object_as_str_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let ptr_to_i8_ptr_type = i8_ptr_type.ptr_type(AddressSpace::default());
        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let fn_type = i8_ptr_type.fn_type(&[obj_ptr_type.into()], false);
        let object_as_str_fn = self.module.add_function("object_as_str", fn_type, None);
        let entry = self.context.append_basic_block(object_as_str_fn, "entry");
        let _ = self.builder.position_at_end(entry);

        let param = object_as_str_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();
        let ptr_to_obj_ptr = self
            .builder
            .build_alloca(obj_ptr_type, "")
            .expect("Could not build alloc.");
        let _ = self.builder.build_store(ptr_to_obj_ptr, param);
        let obj_ptr = self
            .builder
            .build_load(ptr_to_obj_ptr, "")
            .expect("Could not load pointer.");
        let obj_address_fn = self.module.get_function("object_address").unwrap();
        let any_obj_ptr = self
            .builder
            .build_call(obj_address_fn, &[obj_ptr.into()], "")
            .expect("Could not get object address.")
            .as_any_value_enum();
        let ptr_to_val = self
            .builder
            .build_struct_gep(any_obj_ptr.into_pointer_value(), 1, "")
            .expect("Could not get value of AnyType pointer.");
        let ptr_to_str = self
            .builder
            .build_bit_cast(ptr_to_val, ptr_to_i8_ptr_type, "")
            .expect("Could not cast value to string.");
        let str_value = self
            .builder
            .build_load(ptr_to_str.into_pointer_value(), "")
            .expect("Could not load value.");
        let _ = self.builder.build_return(Some(&str_value));

        let _ = self.builder.position_at_end(main_entry);

        Ok(object_as_str_fn.as_any_value_enum())
    }

    fn build_object_as_float_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let float_type = self.context.f64_type();
        let float_ptr_type = float_type.ptr_type(AddressSpace::default());
        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let fn_type = float_type.fn_type(&[obj_ptr_type.into()], false);
        let object_as_float_fn = self.module.add_function("object_as_float", fn_type, None);
        let entry = self.context.append_basic_block(object_as_float_fn, "entry");
        let _ = self.builder.position_at_end(entry);

        let param = object_as_float_fn
            .get_nth_param(0)
            .unwrap()
            .into_pointer_value();
        let ptr_to_obj_ptr = self
            .builder
            .build_alloca(obj_ptr_type, "")
            .expect("Could not build alloc.");
        let _ = self.builder.build_store(ptr_to_obj_ptr, param);
        let obj_ptr = self
            .builder
            .build_load(ptr_to_obj_ptr, "")
            .expect("Could not load pointer.");
        let obj_address_fn = self.module.get_function("object_address").unwrap();
        let any_obj_ptr = self
            .builder
            .build_call(obj_address_fn, &[obj_ptr.into()], "")
            .expect("Could not get object address.")
            .as_any_value_enum();
        let ptr_to_val = self
            .builder
            .build_struct_gep(any_obj_ptr.into_pointer_value(), 1, "")
            .expect("Could not get value of AnyType pointer.");
        let ptr_to_str = self
            .builder
            .build_bit_cast(ptr_to_val, float_ptr_type, "")
            .expect("Could not cast value to float.");
        let str_value = self
            .builder
            .build_load(ptr_to_str.into_pointer_value(), "")
            .expect("Could not load value.");
        let _ = self.builder.build_return(Some(&str_value));

        let _ = self.builder.position_at_end(main_entry);

        Ok(object_as_float_fn.as_any_value_enum())
    }

    fn build_new_str_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let ptr_to_i8_ptr_type = i8_ptr_type.ptr_type(AddressSpace::default());
        let any_ptr_type = self.any_type.ptr_type(AddressSpace::default());
        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let fn_type = obj_ptr_type.fn_type(&[i8_ptr_type.into()], false);
        let new_str_fn = self.module.add_function("new_str", fn_type, None);
        let entry = self.context.append_basic_block(new_str_fn, "entry");
        let _ = self.builder.position_at_end(entry);

        let param = new_str_fn.get_nth_param(0).unwrap().into_pointer_value();

        let ptr_to_any_obj_ptr = self
            .builder
            .build_alloca(any_ptr_type, "")
            .expect("Could not allocate memory.");
        let any_obj_ptr = self
            .builder
            .build_malloc(self.any_type, "")
            .expect("Could not perform malloc.");
        let _ = self.builder.build_store(ptr_to_any_obj_ptr, any_obj_ptr);

        let str_type = self.context.i8_type().const_int(2, false);
        let type_ptr = self
            .builder
            .build_struct_gep(any_obj_ptr, 0, "")
            .expect("Could not get pointer to type tag.");
        let _ = self.builder.build_store(type_ptr, str_type);

        let value_ptr = self
            .builder
            .build_struct_gep(any_obj_ptr, 1, "")
            .expect("Could not get pointer to value of AnyType.");
        let value_ptr_as_str_ptr = self
            .builder
            .build_bit_cast(value_ptr, ptr_to_i8_ptr_type, "")
            .expect("Could not cast pointer to string pointer type.")
            .into_pointer_value();
        let _ = self.builder.build_store(value_ptr_as_str_ptr, param);
        let any_obj = self
            .builder
            .build_load(ptr_to_any_obj_ptr, "")
            .expect("Could not load pointer");
        let obj_from_address = self.module.get_function("object_from_address").unwrap();
        let str_as_obj = self
            .builder
            .build_call(obj_from_address, &[any_obj.into()], "")
            .expect("Could not make call to object_from_address.")
            .as_any_value_enum()
            .into_pointer_value();
        let _ = self.builder.build_return(Some(&str_as_obj));

        let _ = self.builder.position_at_end(main_entry);

        Ok(new_str_fn.as_any_value_enum())
    }

    fn build_new_float_fn(&self) -> IRGenResult<'_> {
        let main_entry = self
            .builder
            .get_insert_block()
            .expect("Builder not attached to entry point.");

        let f64_type = self.context.f64_type();
        let f64_ptr_type = f64_type.ptr_type(AddressSpace::default());
        let any_ptr_type = self.any_type.ptr_type(AddressSpace::default());
        let obj_ptr_type = self.object_type.ptr_type(AddressSpace::default());
        let fn_type = obj_ptr_type.fn_type(&[f64_type.into()], false);
        let new_float_fn = self.module.add_function("new_float", fn_type, None);
        let entry = self.context.append_basic_block(new_float_fn, "entry");
        let _ = self.builder.position_at_end(entry);

        let param = new_float_fn.get_nth_param(0).unwrap().into_float_value();

        let ptr_to_any_obj_ptr = self
            .builder
            .build_alloca(any_ptr_type, "")
            .expect("Could not allocate memory.");
        let any_obj_ptr = self
            .builder
            .build_malloc(self.any_type, "")
            .expect("Could not perform malloc.");
        let _ = self.builder.build_store(ptr_to_any_obj_ptr, any_obj_ptr);

        let float_type = self.context.i8_type().const_int(3, false);
        let type_ptr = self
            .builder
            .build_struct_gep(any_obj_ptr, 0, "")
            .expect("Could not get pointer to type tag.");
        let _ = self.builder.build_store(type_ptr, float_type);

        let value_ptr = self
            .builder
            .build_struct_gep(any_obj_ptr, 1, "")
            .expect("Could not get pointer to value of AnyType.");
        let value_ptr_as_float_ptr = self
            .builder
            .build_bit_cast(value_ptr, f64_ptr_type, "")
            .expect("Could not cast pointer to float pointer type.")
            .into_pointer_value();
        let _ = self.builder.build_store(value_ptr_as_float_ptr, param);
        let any_obj = self
            .builder
            .build_load(ptr_to_any_obj_ptr, "")
            .expect("Could not load pointer");
        let obj_from_address = self.module.get_function("object_from_address").unwrap();
        let float_as_obj = self
            .builder
            .build_call(obj_from_address, &[any_obj.into()], "")
            .expect("Could not make call to object_from_address.")
            .as_any_value_enum()
            .into_pointer_value();
        let _ = self.builder.build_return(Some(&float_as_obj));

        let _ = self.builder.position_at_end(main_entry);

        Ok(new_float_fn.as_any_value_enum())
    }
}

#[derive(Debug)]
struct TypeInfo {
    pub tag: u8,
    pub tag_mask: u8,
    pub shift: i64,
}

impl TypeInfo {
    pub fn new(tag: u8, tag_mask: u8, shift: i64) -> Self {
        Self {
            tag,
            tag_mask,
            shift,
        }
    }
}

/**
 * Map for all Any type stuff.
 */
fn create_type_info_hashmap() -> HashMap<String, TypeInfo> {
    HashMap::from([
        ("Bool".to_string(), TypeInfo::new(0 as u8, 1 as u8, 2)),
        ("Int".to_string(), TypeInfo::new(1 as u8, 1 as u8, 1)),
        ("Heap".to_string(), TypeInfo::new(2 as u8, 2 as u8, 2)),
    ])
}
