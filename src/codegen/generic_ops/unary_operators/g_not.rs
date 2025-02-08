use rustpython_parser::ast::UnaryOpNot;

use crate::codegen::generic_ops::build_generic_op::GenericOpIR;

impl GenericOpIR for UnaryOpNot {
    fn get_generic_op_ir() -> String {
        r#"define dso_local zeroext i1 @obj_as_truthy(%struct.Object* noundef %0) #0 {
  %2 = alloca i1, align 1
  %3 = alloca %struct.Object*, align 8
  %4 = alloca i8, align 1
  %5 = alloca i8, align 1
  %6 = alloca i8*, align 8
  store %struct.Object* %0, %struct.Object** %3, align 8
  %7 = load %struct.Object*, %struct.Object** %3, align 8
  %8 = icmp eq %struct.Object* %7, null
  br i1 %8, label %9, label %10

9:                                                ; preds = %1
  store i1 false, i1* %2, align 1
  br label %36

10:                                               ; preds = %1
  %11 = load %struct.Object*, %struct.Object** %3, align 8
  %12 = call i32 @object_type(%struct.Object* noundef %11)
  switch i32 %12, label %35 [
    i32 0, label %13
    i32 1, label %16
    i32 3, label %23
    i32 2, label %30
  ]

13:                                               ; preds = %10
  %14 = load %struct.Object*, %struct.Object** %3, align 8
  %15 = call zeroext i1 @object_as_bool(%struct.Object* noundef %14)
  store i1 %15, i1* %2, align 1
  br label %36

16:                                               ; preds = %10
  %17 = load %struct.Object*, %struct.Object** %3, align 8
  %18 = call i64 @object_as_int(%struct.Object* noundef %17)
  %19 = icmp ne i64 %18, 0
  %20 = zext i1 %19 to i8
  store i8 %20, i8* %4, align 1
  %21 = load i8, i8* %4, align 1
  %22 = trunc i8 %21 to i1
  store i1 %22, i1* %2, align 1
  br label %36

23:                                               ; preds = %10
  %24 = load %struct.Object*, %struct.Object** %3, align 8
  %25 = call double @object_as_float(%struct.Object* noundef %24)
  %26 = fcmp une double %25, 0.000000e+00
  %27 = zext i1 %26 to i8
  store i8 %27, i8* %5, align 1
  %28 = load i8, i8* %5, align 1
  %29 = trunc i8 %28 to i1
  store i1 %29, i1* %2, align 1
  br label %36

30:                                               ; preds = %10
  %31 = load %struct.Object*, %struct.Object** %3, align 8
  %32 = call i8* @object_as_str(%struct.Object* noundef %31)
  store i8* %32, i8** %6, align 8
  %33 = load i8*, i8** %6, align 8
  %34 = call zeroext i1 @str_is_truthy(i8* noundef %33)
  store i1 %34, i1* %2, align 1
  br label %36

35:                                               ; preds = %10
  store i1 false, i1* %2, align 1
  br label %36

36:                                               ; preds = %35, %30, %23, %16, %13, %9
  %37 = load i1, i1* %2, align 1
  ret i1 %37
}

define dso_local zeroext i1 @Not(%struct.Object* noundef %0) #0 {
  %2 = alloca %struct.Object*, align 8
  store %struct.Object* %0, %struct.Object** %2, align 8
  %3 = load %struct.Object*, %struct.Object** %2, align 8
  %4 = call zeroext i1 @obj_as_truthy(%struct.Object* noundef %3)
  %5 = xor i1 %4, true
  ret i1 %5
}"#.to_string()
    }
}