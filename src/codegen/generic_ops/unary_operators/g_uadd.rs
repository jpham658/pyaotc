use rustpython_parser::ast::UnaryOpUAdd;

use crate::codegen::generic_ops::build_generic_op::GenericOpIR;

impl GenericOpIR for UnaryOpUAdd {
    fn get_generic_op_ir() -> String {
        r#"define dso_local %struct.Object* @UAdd(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  %3 = alloca %struct.Object*, align 8
  %4 = alloca i32, align 4
  %5 = alloca double, align 8
  %6 = alloca i32, align 4
  store %struct.Object* %0, %struct.Object** %3, align 8
  %7 = load %struct.Object*, %struct.Object** %3, align 8
  %8 = call zeroext i1 @object_is_int(%struct.Object* noundef %7)
  br i1 %8, label %9, label %16

9:                                                ; preds = %1
  %10 = load %struct.Object*, %struct.Object** %3, align 8
  %11 = call i64 @object_as_int(%struct.Object* noundef %10)
  %12 = trunc i64 %11 to i32
  store i32 %12, i32* %4, align 4
  %13 = load i32, i32* %4, align 4
  %14 = sext i32 %13 to i64
  %15 = call %struct.Object* @new_int(i64 noundef %14)
  store %struct.Object* %15, %struct.Object** %2, align 8
  br label %35

16:                                               ; preds = %1
  %17 = load %struct.Object*, %struct.Object** %3, align 8
  %18 = call zeroext i1 @object_is_float(%struct.Object* noundef %17)
  br i1 %18, label %19, label %24

19:                                               ; preds = %16
  %20 = load %struct.Object*, %struct.Object** %3, align 8
  %21 = call double @object_as_float(%struct.Object* noundef %20)
  store double %21, double* %5, align 8
  %22 = load double, double* %5, align 8
  %23 = call %struct.Object* @new_float(double noundef %22)
  store %struct.Object* %23, %struct.Object** %2, align 8
  br label %35

24:                                               ; preds = %16
  %25 = load %struct.Object*, %struct.Object** %3, align 8
  %26 = call zeroext i1 @object_is_bool(%struct.Object* noundef %25)
  br i1 %26, label %27, label %34

27:                                               ; preds = %24
  %28 = load %struct.Object*, %struct.Object** %3, align 8
  %29 = call zeroext i1 @object_as_bool(%struct.Object* noundef %28)
  %30 = zext i1 %29 to i32
  store i32 %30, i32* %6, align 4
  %31 = load i32, i32* %6, align 4
  %32 = sext i32 %31 to i64
  %33 = call %struct.Object* @new_int(i64 noundef %32)
  store %struct.Object* %33, %struct.Object** %2, align 8
  br label %35

34:                                               ; preds = %24
  store %struct.Object* null, %struct.Object** %2, align 8
  br label %35

35:                                               ; preds = %34, %27, %19, %9
  %36 = load %struct.Object*, %struct.Object** %2, align 8
  ret %struct.Object* %36
}
  "#
        .to_string()
    }
}
