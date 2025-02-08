use rustpython_parser::ast::UnaryOpUSub;

use crate::codegen::generic_ops::build_generic_op::GenericOpIR;

impl GenericOpIR for UnaryOpUSub {
    fn get_generic_op_ir() -> String {
        r#"define dso_local %struct.Object* @USub(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  %3 = alloca %struct.Object*, align 8
  %4 = alloca i32, align 4
  %5 = alloca double, align 8
  %6 = alloca i32, align 4
  store %struct.Object* %0, %struct.Object** %3, align 8
  %7 = load %struct.Object*, %struct.Object** %3, align 8
  %8 = call zeroext i1 @object_is_int(%struct.Object* noundef %7)
  br i1 %8, label %9, label %17

9:                                                ; preds = %1
  %10 = load %struct.Object*, %struct.Object** %3, align 8
  %11 = call i64 @object_as_int(%struct.Object* noundef %10)
  %12 = trunc i64 %11 to i32
  store i32 %12, i32* %4, align 4
  %13 = load i32, i32* %4, align 4
  %14 = sub nsw i32 0, %13
  %15 = sext i32 %14 to i64
  %16 = call %struct.Object* @new_int(i64 noundef %15)
  store %struct.Object* %16, %struct.Object** %2, align 8
  br label %38

17:                                               ; preds = %1
  %18 = load %struct.Object*, %struct.Object** %3, align 8
  %19 = call zeroext i1 @object_is_float(%struct.Object* noundef %18)
  br i1 %19, label %20, label %26

20:                                               ; preds = %17
  %21 = load %struct.Object*, %struct.Object** %3, align 8
  %22 = call double @object_as_float(%struct.Object* noundef %21)
  store double %22, double* %5, align 8
  %23 = load double, double* %5, align 8
  %24 = fneg double %23
  %25 = call %struct.Object* @new_float(double noundef %24)
  store %struct.Object* %25, %struct.Object** %2, align 8
  br label %38

26:                                               ; preds = %17
  %27 = load %struct.Object*, %struct.Object** %3, align 8
  %28 = call zeroext i1 @object_is_bool(%struct.Object* noundef %27)
  br i1 %28, label %29, label %37

29:                                               ; preds = %26
  %30 = load %struct.Object*, %struct.Object** %3, align 8
  %31 = call zeroext i1 @object_as_bool(%struct.Object* noundef %30)
  %32 = zext i1 %31 to i32
  store i32 %32, i32* %6, align 4
  %33 = load i32, i32* %6, align 4
  %34 = sub nsw i32 0, %33
  %35 = sext i32 %34 to i64
  %36 = call %struct.Object* @new_int(i64 noundef %35)
  store %struct.Object* %36, %struct.Object** %2, align 8
  br label %38

37:                                               ; preds = %26
  store %struct.Object* null, %struct.Object** %2, align 8
  br label %38

38:                                               ; preds = %37, %29, %20, %9
  %39 = load %struct.Object*, %struct.Object** %2, align 8
  ret %struct.Object* %39
}
        "#
        .to_string()
    }
}
