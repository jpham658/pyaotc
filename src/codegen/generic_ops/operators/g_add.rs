use rustpython_parser::ast::OperatorAdd;

use crate::codegen::generic_ops::build_generic_op::GenericOpIR;

impl GenericOpIR for OperatorAdd {
    fn get_generic_op_ir() -> String {
        r#"
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg)

define %struct.Object* @Add(%struct.Object* %0, %struct.Object* %1) {
entry:
  %sum = alloca %struct.Object*, align 8
  %2 = call i1 @object_is_int(%struct.Object* %0)
  %3 = call i1 @object_is_bool(%struct.Object* %0)
  %4 = call i1 @object_is_float(%struct.Object* %0)
  %5 = call i1 @object_is_str(%struct.Object* %0)
  %6 = call i1 @object_is_int(%struct.Object* %1)
  %7 = call i1 @object_is_bool(%struct.Object* %1)
  %8 = call i1 @object_is_float(%struct.Object* %1)
  %9 = call i1 @object_is_str(%struct.Object* %1)
  %int_int = and i1 %2, %6
  %int_float = and i1 %2, %8
  %float_int = and i1 %4, %6
  %float_float = and i1 %4, %8
  %int_bool = and i1 %2, %7
  %bool_int = and i1 %3, %6
  %float_bool = and i1 %4, %7
  %bool_float = and i1 %3, %8
  %bool_bool = and i1 %3, %7
  %str_str = and i1 %5, %9
  br i1 %int_int, label %handle_int_int, label %test_int_float

handle_int_int:                                   ; preds = %entry
  %10 = call i64 @object_as_int(%struct.Object* %0)
  %11 = call i64 @object_as_int(%struct.Object* %1)
  %iadd = add i64 %10, %11
  %12 = call %struct.Object* @new_int(i64 %iadd)
  store %struct.Object* %12, %struct.Object** %sum, align 8
  br label %merge

test_int_float:                                   ; preds = %entry
  br i1 %int_float, label %handle_int_float, label %test_float_int

handle_int_float:                                 ; preds = %test_int_float
  %13 = call i64 @object_as_int(%struct.Object* %0)
  %14 = call double @object_as_float(%struct.Object* %1)
  %15 = sitofp i64 %13 to double
  %fadd = fadd double %15, %14
  %16 = call %struct.Object* @new_float(double %fadd)
  store %struct.Object* %16, %struct.Object** %sum, align 8
  br label %merge

test_float_int:                                   ; preds = %test_int_float
  br i1 %float_int, label %handle_float_int, label %test_float_float

handle_float_int:                                 ; preds = %test_float_int
  %17 = call double @object_as_float(%struct.Object* %0)
  %18 = call i64 @object_as_int(%struct.Object* %1)
  %19 = sitofp i64 %18 to double
  %fadd1 = fadd double %17, %19
  %20 = call %struct.Object* @new_float(double %fadd1)
  store %struct.Object* %20, %struct.Object** %sum, align 8
  br label %merge

test_float_float:                                 ; preds = %test_float_int
  br i1 %float_float, label %handle_float_float, label %test_int_bool

handle_float_float:                               ; preds = %test_float_float
  %21 = call double @object_as_float(%struct.Object* %0)
  %22 = call double @object_as_float(%struct.Object* %1)
  %fadd2 = fadd double %21, %22
  %23 = call %struct.Object* @new_float(double %fadd2)
  store %struct.Object* %23, %struct.Object** %sum, align 8
  br label %merge

test_int_bool:                                    ; preds = %test_float_float
  br i1 %int_bool, label %handle_int_bool, label %test_bool_int

handle_int_bool:                                  ; preds = %test_int_bool
  %24 = call i64 @object_as_int(%struct.Object* %0)
  %25 = call i1 @object_as_bool(%struct.Object* %1)
  %26 = zext i1 %25 to i64
  %iadd3 = add i64 %24, %26
  %27 = call %struct.Object* @new_int(i64 %iadd3)
  store %struct.Object* %27, %struct.Object** %sum, align 8
  br label %merge

test_bool_int:                                    ; preds = %test_int_bool
  br i1 %bool_int, label %handle_bool_int, label %test_float_bool

handle_bool_int:                                  ; preds = %test_bool_int
  %28 = call i1 @object_as_bool(%struct.Object* %0)
  %29 = call i64 @object_as_int(%struct.Object* %1)
  %30 = zext i1 %28 to i64
  %iadd4 = add i64 %30, %29
  %31 = call %struct.Object* @new_int(i64 %iadd4)
  store %struct.Object* %31, %struct.Object** %sum, align 8
  br label %merge

test_float_bool:                                  ; preds = %test_bool_int
  br i1 %float_bool, label %handle_float_bool, label %test_bool_float

handle_float_bool:                                ; preds = %test_float_bool
  %32 = call double @object_as_float(%struct.Object* %0)
  %33 = call i1 @object_as_bool(%struct.Object* %1)
  %34 = uitofp i1 %33 to double
  %fadd5 = fadd double %32, %34
  %35 = call %struct.Object* @new_float(double %fadd5)
  store %struct.Object* %35, %struct.Object** %sum, align 8
  br label %merge

test_bool_float:                                  ; preds = %test_float_bool
  br i1 %bool_float, label %handle_bool_float, label %test_bool_bool

handle_bool_float:                                ; preds = %test_bool_float
  %36 = call i1 @object_as_bool(%struct.Object* %0)
  %37 = call double @object_as_float(%struct.Object* %1)
  %38 = uitofp i1 %36 to double
  %fadd6 = fadd double %38, %37
  %39 = call %struct.Object* @new_float(double %fadd6)
  store %struct.Object* %39, %struct.Object** %sum, align 8
  br label %merge

test_bool_bool:                                   ; preds = %test_bool_float
  br i1 %bool_bool, label %handle_bool_bool, label %test_str_str

handle_bool_bool:                                 ; preds = %test_bool_bool
  %40 = call i1 @object_as_bool(%struct.Object* %0)
  %41 = call i1 @object_as_bool(%struct.Object* %1)
  %42 = zext i1 %40 to i64
  %43 = zext i1 %41 to i64
  %iadd7 = add i64 %42, %43
  %44 = call %struct.Object* @new_int(i64 %iadd7)
  store %struct.Object* %44, %struct.Object** %sum, align 8
  br label %merge

test_str_str:
  br i1 %str_str, label %handle_str_str, label %unreachable

handle_str_str:
  %45 = alloca [1 x i8], align 1
  %46 = call i8* @object_as_str(%struct.Object* %0)
  %47 = call i8* @object_as_str(%struct.Object* %1)
  %48 = call i8* @strconcat(i8* %46, i8* %47)
  %49 = call %struct.Object* @new_str(i8* %48)
  store %struct.Object* %49, %struct.Object** %sum, align 8
  br label %merge

unreachable:                                      ; preds = %test_str_str
  unreachable

merge:                                            ; preds = %handle_bool_bool, %handle_bool_float, %handle_float_bool, %handle_bool_int, %handle_int_bool, %handle_float_float, %handle_float_int, %handle_int_float, %handle_int_int
  %sum_obj = load %struct.Object*, %struct.Object** %sum, align 8
  ret %struct.Object* %sum_obj
}
"#.to_string()
    }
}
