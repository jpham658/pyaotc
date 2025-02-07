use rustpython_parser::ast::OperatorSub;

use crate::codegen::generic_ops::build_generic_op::GenericOpIR;

impl GenericOpIR for OperatorSub {
    fn get_generic_op_ir() -> String {
        r#"
define dso_local %struct.Object* @Sub(%struct.Object* noundef %0, %struct.Object* noundef %1) #0 {
  %3 = alloca %struct.Object*, align 8
  %4 = alloca %struct.Object*, align 8
  %5 = alloca %struct.Object*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca double, align 8
  %9 = alloca double, align 8
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  %12 = alloca double, align 8
  %13 = alloca double, align 8
  %14 = alloca double, align 8
  %15 = alloca double, align 8
  %16 = alloca i32, align 4
  %17 = alloca i32, align 4
  %18 = alloca i32, align 4
  %19 = alloca i32, align 4
  %20 = alloca double, align 8
  %21 = alloca double, align 8
  %22 = alloca double, align 8
  %23 = alloca double, align 8
  store %struct.Object* %0, %struct.Object** %4, align 8
  store %struct.Object* %1, %struct.Object** %5, align 8
  %24 = load %struct.Object*, %struct.Object** %4, align 8
  %25 = call zeroext i1 @object_is_int(%struct.Object* noundef %24)
  br i1 %25, label %26, label %41

26:                                               ; preds = %2
  %27 = load %struct.Object*, %struct.Object** %5, align 8
  %28 = call zeroext i1 @object_is_int(%struct.Object* noundef %27)
  br i1 %28, label %29, label %41

29:                                               ; preds = %26
  %30 = load %struct.Object*, %struct.Object** %4, align 8
  %31 = call i64 @object_as_int(%struct.Object* noundef %30)
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %6, align 4
  %33 = load %struct.Object*, %struct.Object** %5, align 8
  %34 = call i64 @object_as_int(%struct.Object* noundef %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %7, align 4
  %36 = load i32, i32* %6, align 4
  %37 = load i32, i32* %7, align 4
  %38 = sub nsw i32 %36, %37
  %39 = sext i32 %38 to i64
  %40 = call %struct.Object* @new_int(i64 noundef %39)
  store %struct.Object* %40, %struct.Object** %3, align 8
  br label %175

41:                                               ; preds = %26, %2
  %42 = load %struct.Object*, %struct.Object** %4, align 8
  %43 = call zeroext i1 @object_is_float(%struct.Object* noundef %42)
  br i1 %43, label %44, label %56

44:                                               ; preds = %41
  %45 = load %struct.Object*, %struct.Object** %5, align 8
  %46 = call zeroext i1 @object_is_float(%struct.Object* noundef %45)
  br i1 %46, label %47, label %56

47:                                               ; preds = %44
  %48 = load %struct.Object*, %struct.Object** %4, align 8
  %49 = call double @object_as_float(%struct.Object* noundef %48)
  store double %49, double* %8, align 8
  %50 = load %struct.Object*, %struct.Object** %5, align 8
  %51 = call double @object_as_float(%struct.Object* noundef %50)
  store double %51, double* %9, align 8
  %52 = load double, double* %8, align 8
  %53 = load double, double* %9, align 8
  %54 = fsub double %52, %53
  %55 = call %struct.Object* @new_float(double noundef %54)
  store %struct.Object* %55, %struct.Object** %3, align 8
  br label %175

56:                                               ; preds = %44, %41
  %57 = load %struct.Object*, %struct.Object** %4, align 8
  %58 = call zeroext i1 @object_is_bool(%struct.Object* noundef %57)
  br i1 %58, label %59, label %74

59:                                               ; preds = %56
  %60 = load %struct.Object*, %struct.Object** %5, align 8
  %61 = call zeroext i1 @object_is_bool(%struct.Object* noundef %60)
  br i1 %61, label %62, label %74

62:                                               ; preds = %59
  %63 = load %struct.Object*, %struct.Object** %4, align 8
  %64 = call zeroext i1 @object_as_bool(%struct.Object* noundef %63)
  %65 = zext i1 %64 to i32
  store i32 %65, i32* %10, align 4
  %66 = load %struct.Object*, %struct.Object** %5, align 8
  %67 = call zeroext i1 @object_as_bool(%struct.Object* noundef %66)
  %68 = zext i1 %67 to i32
  store i32 %68, i32* %11, align 4
  %69 = load i32, i32* %10, align 4
  %70 = load i32, i32* %11, align 4
  %71 = sub nsw i32 %69, %70
  %72 = sext i32 %71 to i64
  %73 = call %struct.Object* @new_int(i64 noundef %72)
  store %struct.Object* %73, %struct.Object** %3, align 8
  br label %175

74:                                               ; preds = %59, %56
  %75 = load %struct.Object*, %struct.Object** %4, align 8
  %76 = call zeroext i1 @object_is_float(%struct.Object* noundef %75)
  br i1 %76, label %77, label %90

77:                                               ; preds = %74
  %78 = load %struct.Object*, %struct.Object** %5, align 8
  %79 = call zeroext i1 @object_is_int(%struct.Object* noundef %78)
  br i1 %79, label %80, label %90

80:                                               ; preds = %77
  %81 = load %struct.Object*, %struct.Object** %4, align 8
  %82 = call double @object_as_float(%struct.Object* noundef %81)
  store double %82, double* %12, align 8
  %83 = load %struct.Object*, %struct.Object** %5, align 8
  %84 = call i64 @object_as_int(%struct.Object* noundef %83)
  %85 = sitofp i64 %84 to double
  store double %85, double* %13, align 8
  %86 = load double, double* %12, align 8
  %87 = load double, double* %13, align 8
  %88 = fsub double %86, %87
  %89 = call %struct.Object* @new_float(double noundef %88)
  store %struct.Object* %89, %struct.Object** %3, align 8
  br label %175

90:                                               ; preds = %77, %74
  %91 = load %struct.Object*, %struct.Object** %4, align 8
  %92 = call zeroext i1 @object_is_int(%struct.Object* noundef %91)
  br i1 %92, label %93, label %106

93:                                               ; preds = %90
  %94 = load %struct.Object*, %struct.Object** %5, align 8
  %95 = call zeroext i1 @object_is_float(%struct.Object* noundef %94)
  br i1 %95, label %96, label %106

96:                                               ; preds = %93
  %97 = load %struct.Object*, %struct.Object** %4, align 8
  %98 = call i64 @object_as_int(%struct.Object* noundef %97)
  %99 = sitofp i64 %98 to double
  store double %99, double* %14, align 8
  %100 = load %struct.Object*, %struct.Object** %5, align 8
  %101 = call double @object_as_float(%struct.Object* noundef %100)
  store double %101, double* %15, align 8
  %102 = load double, double* %14, align 8
  %103 = load double, double* %15, align 8
  %104 = fsub double %102, %103
  %105 = call %struct.Object* @new_float(double noundef %104)
  store %struct.Object* %105, %struct.Object** %3, align 8
  br label %175

106:                                              ; preds = %93, %90
  %107 = load %struct.Object*, %struct.Object** %4, align 8
  %108 = call zeroext i1 @object_is_bool(%struct.Object* noundef %107)
  br i1 %108, label %109, label %124

109:                                              ; preds = %106
  %110 = load %struct.Object*, %struct.Object** %5, align 8
  %111 = call zeroext i1 @object_is_int(%struct.Object* noundef %110)
  br i1 %111, label %112, label %124

112:                                              ; preds = %109
  %113 = load %struct.Object*, %struct.Object** %4, align 8
  %114 = call zeroext i1 @object_as_bool(%struct.Object* noundef %113)
  %115 = zext i1 %114 to i32
  store i32 %115, i32* %16, align 4
  %116 = load %struct.Object*, %struct.Object** %5, align 8
  %117 = call i64 @object_as_int(%struct.Object* noundef %116)
  %118 = trunc i64 %117 to i32
  store i32 %118, i32* %17, align 4
  %119 = load i32, i32* %16, align 4
  %120 = load i32, i32* %17, align 4
  %121 = sub nsw i32 %119, %120
  %122 = sext i32 %121 to i64
  %123 = call %struct.Object* @new_int(i64 noundef %122)
  store %struct.Object* %123, %struct.Object** %3, align 8
  br label %175

124:                                              ; preds = %109, %106
  %125 = load %struct.Object*, %struct.Object** %4, align 8
  %126 = call zeroext i1 @object_is_int(%struct.Object* noundef %125)
  br i1 %126, label %127, label %142

127:                                              ; preds = %124
  %128 = load %struct.Object*, %struct.Object** %5, align 8
  %129 = call zeroext i1 @object_is_bool(%struct.Object* noundef %128)
  br i1 %129, label %130, label %142

130:                                              ; preds = %127
  %131 = load %struct.Object*, %struct.Object** %4, align 8
  %132 = call i64 @object_as_int(%struct.Object* noundef %131)
  %133 = trunc i64 %132 to i32
  store i32 %133, i32* %18, align 4
  %134 = load %struct.Object*, %struct.Object** %5, align 8
  %135 = call zeroext i1 @object_as_bool(%struct.Object* noundef %134)
  %136 = zext i1 %135 to i32
  store i32 %136, i32* %19, align 4
  %137 = load i32, i32* %18, align 4
  %138 = load i32, i32* %19, align 4
  %139 = sub nsw i32 %137, %138
  %140 = sext i32 %139 to i64
  %141 = call %struct.Object* @new_int(i64 noundef %140)
  store %struct.Object* %141, %struct.Object** %3, align 8
  br label %175

142:                                              ; preds = %127, %124
  %143 = load %struct.Object*, %struct.Object** %4, align 8
  %144 = call zeroext i1 @object_is_bool(%struct.Object* noundef %143)
  br i1 %144, label %145, label %158

145:                                              ; preds = %142
  %146 = load %struct.Object*, %struct.Object** %5, align 8
  %147 = call zeroext i1 @object_is_float(%struct.Object* noundef %146)
  br i1 %147, label %148, label %158

148:                                              ; preds = %145
  %149 = load %struct.Object*, %struct.Object** %4, align 8
  %150 = call zeroext i1 @object_as_bool(%struct.Object* noundef %149)
  %151 = uitofp i1 %150 to double
  store double %151, double* %20, align 8
  %152 = load %struct.Object*, %struct.Object** %5, align 8
  %153 = call double @object_as_float(%struct.Object* noundef %152)
  store double %153, double* %21, align 8
  %154 = load double, double* %20, align 8
  %155 = load double, double* %21, align 8
  %156 = fsub double %154, %155
  %157 = call %struct.Object* @new_float(double noundef %156)
  store %struct.Object* %157, %struct.Object** %3, align 8
  br label %175

158:                                              ; preds = %145, %142
  %159 = load %struct.Object*, %struct.Object** %4, align 8
  %160 = call zeroext i1 @object_is_float(%struct.Object* noundef %159)
  br i1 %160, label %161, label %174

161:                                              ; preds = %158
  %162 = load %struct.Object*, %struct.Object** %5, align 8
  %163 = call zeroext i1 @object_is_bool(%struct.Object* noundef %162)
  br i1 %163, label %164, label %174

164:                                              ; preds = %161
  %165 = load %struct.Object*, %struct.Object** %4, align 8
  %166 = call double @object_as_float(%struct.Object* noundef %165)
  store double %166, double* %22, align 8
  %167 = load %struct.Object*, %struct.Object** %5, align 8
  %168 = call zeroext i1 @object_as_bool(%struct.Object* noundef %167)
  %169 = uitofp i1 %168 to double
  store double %169, double* %23, align 8
  %170 = load double, double* %22, align 8
  %171 = load double, double* %23, align 8
  %172 = fsub double %170, %171
  %173 = call %struct.Object* @new_float(double noundef %172)
  store %struct.Object* %173, %struct.Object** %3, align 8
  br label %175

174:                                              ; preds = %161, %158
  store %struct.Object* null, %struct.Object** %3, align 8
  br label %175

175:                                              ; preds = %174, %164, %148, %130, %112, %96, %80, %62, %47, %29
  %176 = load %struct.Object*, %struct.Object** %3, align 8
  ret %struct.Object* %176
}
  "#.to_string()
    }
}
