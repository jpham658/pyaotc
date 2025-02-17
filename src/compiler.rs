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
use crate::codegen::generic_ops::build_generic_op::{
    get_generic_cmp_ops_ir, get_generic_ops_ir, get_generic_uops_ir,
};
use crate::codegen::typed_codegen::LLVMTypedCodegen;
use crate::type_inference::TypeEnv;

// TODO: Move this to separate file
fn get_prereq_llvm_ir() -> String {
    let cmp_op_ir = get_generic_cmp_ops_ir();
    let op_ir = get_generic_ops_ir();
    let uop_ir = get_generic_uops_ir();
    r#"%struct.Object = type opaque
%struct.HeapObject = type { i32, %union.anon }
%union.anon = type { i8* }
%struct.__va_list_tag = type { i32, i32, i8*, i8* }

declare i8* @malloc(i64 noundef) #1
declare i64 @strlen(i8* noundef)
declare i8* @strcpy(i8* noundef, i8* noundef)
declare i8* @strcat(i8* noundef, i8* noundef)
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg)
declare void @llvm.va_start(i8*) #3
declare void @llvm.va_end(i8*) #3

@.int_str_format = private unnamed_addr constant [4 x i8] c"%d \00", align 1
@.true_str_format = private unnamed_addr constant [6 x i8] c"True \00", align 1
@.false_str_format = private unnamed_addr constant [7 x i8] c"False \00", align 1
@.str_format = private unnamed_addr constant [4 x i8] c"%s \00", align 1
@.float_str_format = private unnamed_addr constant [4 x i8] c"%f \00", align 1
@.invalid_heap_obj_str = private unnamed_addr constant [23 x i8] c"Not valid heap object.\00", align 1
@.empty_str = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@.newline = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.none_str = private unnamed_addr constant [6 x i8] c"None \00", align 1

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
define dso_local void @print_int(i64 noundef %0) #0 {
  %2 = alloca i64, align 4
  store i64 %0, i64* %2, align 4
  %3 = load i64, i64* %2, align 4
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.int_str_format, i64 0, i64 0), i64 noundef %3)
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
  %8 = select i1 %6, i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.true_str_format, i64 0, i64 0), i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.false_str_format, i64 0, i64 0)
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
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.str_format, i64 0, i64 0), i8* noundef %3)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_float(double noundef %0) #0 {
  %2 = alloca double, align 8
  store double %0, double* %2, align 8
  %3 = load double, double* %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([4 x i8], [4 x i8]* @.float_str_format, i64 0, i64 0), double noundef %3)
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_newline() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([2 x i8], [2 x i8]* @.newline, i64 0, i64 0))
  ret void
}

define dso_local void @print_none() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([6 x i8], [6 x i8]* @.none_str, i64 0, i64 0))
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @print_obj(i32 noundef %0, %struct.Object* noundef %1, ...) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.Object*, align 8
  %5 = alloca [1 x %struct.__va_list_tag], align 16
  %6 = alloca %struct.Object*, align 8
  %7 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store %struct.Object* %1, %struct.Object** %4, align 8
  %8 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %5, i64 0, i64 0
  %9 = bitcast %struct.__va_list_tag* %8 to i8*
  call void @llvm.va_start(i8* %9)
  %10 = load %struct.Object*, %struct.Object** %4, align 8
  store %struct.Object* %10, %struct.Object** %6, align 8
  store i32 0, i32* %7, align 4
  br label %11

11:                                               ; preds = %54, %2
  %12 = load i32, i32* %7, align 4
  %13 = load i32, i32* %3, align 4
  %14 = icmp slt i32 %12, %13
  br i1 %14, label %15, label %57

15:                                               ; preds = %11
  %16 = load %struct.Object*, %struct.Object** %6, align 8
  %17 = icmp eq %struct.Object* %16, null
  br i1 %17, label %18, label %19

18:                                               ; preds = %15
  call void @print_none()
  br label %54

19:                                               ; preds = %15
  %20 = load %struct.Object*, %struct.Object** %6, align 8
  %21 = call i32 @object_type(%struct.Object* noundef %20)
  switch i32 %21, label %32 [
    i32 1, label %22
    i32 0, label %26
    i32 3, label %29
  ]

22:                                               ; preds = %19
  %23 = load %struct.Object*, %struct.Object** %6, align 8
  %24 = call i64 @object_as_int(%struct.Object* noundef %23)
  %25 = trunc i64 %24 to i32
  call void @print_int(i64 noundef %24)
  br label %35

26:                                               ; preds = %19
  %27 = load %struct.Object*, %struct.Object** %6, align 8
  %28 = call zeroext i1 @object_as_bool(%struct.Object* noundef %27)
  call void @print_bool(i1 noundef zeroext %28)
  br label %35

29:                                               ; preds = %19
  %30 = load %struct.Object*, %struct.Object** %6, align 8
  %31 = call double @object_as_float(%struct.Object* noundef %30)
  call void @print_float(double noundef %31)
  br label %35

32:                                               ; preds = %19
  %33 = load %struct.Object*, %struct.Object** %6, align 8
  %34 = call i8* @object_as_str(%struct.Object* noundef %33)
  call void @print_str(i8* noundef %34)
  br label %35

35:                                               ; preds = %32, %29, %26, %22
  %36 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %5, i64 0, i64 0
  %37 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %36, i32 0, i32 0
  %38 = load i32, i32* %37, align 16
  %39 = icmp ule i32 %38, 40
  br i1 %39, label %40, label %46

40:                                               ; preds = %35
  %41 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %36, i32 0, i32 3
  %42 = load i8*, i8** %41, align 16
  %43 = getelementptr i8, i8* %42, i32 %38
  %44 = bitcast i8* %43 to %struct.Object**
  %45 = add i32 %38, 8
  store i32 %45, i32* %37, align 16
  br label %51

46:                                               ; preds = %35
  %47 = getelementptr inbounds %struct.__va_list_tag, %struct.__va_list_tag* %36, i32 0, i32 2
  %48 = load i8*, i8** %47, align 8
  %49 = bitcast i8* %48 to %struct.Object**
  %50 = getelementptr i8, i8* %48, i32 8
  store i8* %50, i8** %47, align 8
  br label %51

51:                                               ; preds = %46, %40
  %52 = phi %struct.Object** [ %44, %40 ], [ %49, %46 ]
  %53 = load %struct.Object*, %struct.Object** %52, align 8
  store %struct.Object* %53, %struct.Object** %6, align 8
  br label %54

54:                                               ; preds = %51, %18
  %55 = load i32, i32* %7, align 4
  %56 = add nsw i32 %55, 1
  store i32 %56, i32* %7, align 4
  br label %11

57:                                               ; preds = %11
  %58 = getelementptr inbounds [1 x %struct.__va_list_tag], [1 x %struct.__va_list_tag]* %5, i64 0, i64 0
  %59 = bitcast %struct.__va_list_tag* %58 to i8*
  call void @llvm.va_end(i8* %59)
  call void @print_newline()
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
  %19 = call i32 (i8*, ...) @printf(i8* noundef getelementptr inbounds ([23 x i8], [23 x i8]* @.invalid_heap_obj_str, i64 0, i64 0))
  br label %20

20:                                               ; preds = %18, %14
  br label %21

21:                                               ; preds = %20, %6
  call void @print_newline()
  ret void
}

define i8* @strconcat(i8* %str1, i8* %str2) {
entry:
  %0 = call i64 @strlen(i8* %str1)
  %1 = call i64 @strlen(i8* %str2)
  %2 = add i64 %0, %1
  %3 = add i64 %2, 1
  %4 = call i8* @malloc(i64 %3)
  call i8* @strcpy(i8* %4, i8* %str1)
  call i8* @strcat(i8* %4, i8* %str2)
  ret i8* %4
}  

define dso_local i8* @strmult(i8* noundef %0, i32 noundef %1) #0 {
  %3 = alloca i8*, align 8
  %4 = alloca i8*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i64, align 8
  %7 = alloca i8*, align 8
  %8 = alloca i32, align 4
  store i8* %0, i8** %4, align 8
  store i32 %1, i32* %5, align 4
  %9 = load i32, i32* %5, align 4
  %10 = icmp sle i32 %9, 0
  br i1 %10, label %14, label %11

11:                                               ; preds = %2
  %12 = load i8*, i8** %4, align 8
  %13 = icmp eq i8* %12, null
  br i1 %13, label %14, label %15

14:                                               ; preds = %11, %2
  store i8* null, i8** %3, align 8
  br label %52

15:                                               ; preds = %11
  %16 = load i8*, i8** %4, align 8
  %17 = call i64 @strlen(i8* noundef %16) #4
  store i64 %17, i64* %6, align 8
  %18 = load i64, i64* %6, align 8
  %19 = load i32, i32* %5, align 4
  %20 = sext i32 %19 to i64
  %21 = mul i64 %18, %20
  %22 = add i64 %21, 1
  %23 = call i8* @malloc(i64 noundef %22)
  store i8* %23, i8** %7, align 8
  %24 = load i8*, i8** %7, align 8
  %25 = icmp eq i8* %24, null
  br i1 %25, label %26, label %27

26:                                               ; preds = %15
  store i8* null, i8** %3, align 8
  br label %52

27:                                               ; preds = %15
  store i32 0, i32* %8, align 4
  br label %28

28:                                               ; preds = %41, %27
  %29 = load i32, i32* %8, align 4
  %30 = load i32, i32* %5, align 4
  %31 = icmp slt i32 %29, %30
  br i1 %31, label %32, label %44

32:                                               ; preds = %28
  %33 = load i8*, i8** %7, align 8
  %34 = load i32, i32* %8, align 4
  %35 = sext i32 %34 to i64
  %36 = load i64, i64* %6, align 8
  %37 = mul i64 %35, %36
  %38 = getelementptr inbounds i8, i8* %33, i64 %37
  %39 = load i8*, i8** %4, align 8
  %40 = load i64, i64* %6, align 8
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 1 %38, i8* align 1 %39, i64 %40, i1 false)
  br label %41

41:                                               ; preds = %32
  %42 = load i32, i32* %8, align 4
  %43 = add nsw i32 %42, 1
  store i32 %43, i32* %8, align 4
  br label %28

44:                                               ; preds = %28
  %45 = load i8*, i8** %7, align 8
  %46 = load i64, i64* %6, align 8
  %47 = load i32, i32* %5, align 4
  %48 = sext i32 %47 to i64
  %49 = mul i64 %46, %48
  %50 = getelementptr inbounds i8, i8* %45, i64 %49
  store i8 0, i8* %50, align 1
  %51 = load i8*, i8** %7, align 8
  store i8* %51, i8** %3, align 8
  br label %52

52:                                               ; preds = %44, %26, %14
  %53 = load i8*, i8** %3, align 8
  ret i8* %53
}

define dso_local zeroext i1 @str_is_truthy(i8* noundef %0) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = call i32 @strcmp(i8* noundef %3, i8* noundef getelementptr inbounds ([1 x i8], [1 x i8]* @.empty_str, i64 0, i64 0)) #4
  %5 = icmp eq i32 %4, 0
  %6 = xor i1 %5, true
  ret i1 %6
}
"#.to_string() + &cmp_op_ir + &op_ir + &uop_ir
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
    pub object_type: StructType<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let prereq_llvm_ir = get_prereq_llvm_ir();
        let mem_buffer = MemoryBuffer::create_from_memory_range_copy(prereq_llvm_ir.as_bytes(), "");
        let module = context
            .create_module_from_ir(mem_buffer)
            .expect("Could not create module from LLVM IR.");

        let i8_type = context.i8_type();

        let any_type = context.get_struct_type("struct.HeapObject").unwrap();
        let object_type = context.get_struct_type("struct.Object").unwrap(); // Should not fail if context is set up properly.
        let list_type = context.struct_type(&[], false);

        Self {
            context,
            builder,
            module,
            sym_table: RefCell::new(HashMap::new()),
            sym_table_as_any: RefCell::new(HashMap::new()),
            func_args: RefCell::new(HashMap::new()),
            any_type,
            object_type,
        }
    }

    pub fn compile(&self, ast: &[Stmt], types: &TypeEnv) {
        let i32_type = self.context.i32_type();
        // self.setup_compiler();

        let main = self
            .module
            .add_function("main", i32_type.fn_type(&[], false), None);
        let main_entry = self.context.append_basic_block(main, "entry");

        self.builder.position_at_end(main_entry);

        // self.setup_tagged_ptr_fns();

        // Initialise Boehm GC
        self.setup_gc_fns();
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
        // self.setup_compiler();

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

        // self.dump_module();

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
}
