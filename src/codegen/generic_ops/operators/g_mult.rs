use rustpython_parser::ast::OperatorMult;

use crate::codegen::generic_ops::build_generic_op::GenericOpIR;

impl GenericOpIR for OperatorMult {
    fn get_generic_op_ir() -> String {
        r#"
define dso_local %struct.Object* @Mult(%struct.Object* noundef %0, %struct.Object* noundef %1) #0 {
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
  %24 = alloca i8*, align 8
  %25 = alloca i32, align 4
  %26 = alloca i8*, align 8
  store %struct.Object* %0, %struct.Object** %4, align 8
  store %struct.Object* %1, %struct.Object** %5, align 8
  %27 = load %struct.Object*, %struct.Object** %4, align 8
  %28 = call zeroext i1 @object_is_int(%struct.Object* noundef %27)
  br i1 %28, label %29, label %44

29:                                               ; preds = %2
  %30 = load %struct.Object*, %struct.Object** %5, align 8
  %31 = call zeroext i1 @object_is_int(%struct.Object* noundef %30)
  br i1 %31, label %32, label %44

32:                                               ; preds = %29
  %33 = load %struct.Object*, %struct.Object** %4, align 8
  %34 = call i64 @object_as_int(%struct.Object* noundef %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %6, align 4
  %36 = load %struct.Object*, %struct.Object** %5, align 8
  %37 = call i64 @object_as_int(%struct.Object* noundef %36)
  %38 = trunc i64 %37 to i32
  store i32 %38, i32* %7, align 4
  %39 = load i32, i32* %6, align 4
  %40 = load i32, i32* %7, align 4
  %41 = mul nsw i32 %39, %40
  %42 = sext i32 %41 to i64
  %43 = call %struct.Object* @new_int(i64 noundef %42)
  store %struct.Object* %43, %struct.Object** %3, align 8
  br label %195

44:                                               ; preds = %29, %2
  %45 = load %struct.Object*, %struct.Object** %4, align 8
  %46 = call zeroext i1 @object_is_float(%struct.Object* noundef %45)
  br i1 %46, label %47, label %59

47:                                               ; preds = %44
  %48 = load %struct.Object*, %struct.Object** %5, align 8
  %49 = call zeroext i1 @object_is_float(%struct.Object* noundef %48)
  br i1 %49, label %50, label %59

50:                                               ; preds = %47
  %51 = load %struct.Object*, %struct.Object** %4, align 8
  %52 = call double @object_as_float(%struct.Object* noundef %51)
  store double %52, double* %8, align 8
  %53 = load %struct.Object*, %struct.Object** %5, align 8
  %54 = call double @object_as_float(%struct.Object* noundef %53)
  store double %54, double* %9, align 8
  %55 = load double, double* %8, align 8
  %56 = load double, double* %9, align 8
  %57 = fmul double %55, %56
  %58 = call %struct.Object* @new_float(double noundef %57)
  store %struct.Object* %58, %struct.Object** %3, align 8
  br label %195

59:                                               ; preds = %47, %44
  %60 = load %struct.Object*, %struct.Object** %4, align 8
  %61 = call zeroext i1 @object_is_bool(%struct.Object* noundef %60)
  br i1 %61, label %62, label %77

62:                                               ; preds = %59
  %63 = load %struct.Object*, %struct.Object** %5, align 8
  %64 = call zeroext i1 @object_is_bool(%struct.Object* noundef %63)
  br i1 %64, label %65, label %77

65:                                               ; preds = %62
  %66 = load %struct.Object*, %struct.Object** %4, align 8
  %67 = call zeroext i1 @object_as_bool(%struct.Object* noundef %66)
  %68 = zext i1 %67 to i32
  store i32 %68, i32* %10, align 4
  %69 = load %struct.Object*, %struct.Object** %5, align 8
  %70 = call zeroext i1 @object_as_bool(%struct.Object* noundef %69)
  %71 = zext i1 %70 to i32
  store i32 %71, i32* %11, align 4
  %72 = load i32, i32* %10, align 4
  %73 = load i32, i32* %11, align 4
  %74 = mul nsw i32 %72, %73
  %75 = sext i32 %74 to i64
  %76 = call %struct.Object* @new_int(i64 noundef %75)
  store %struct.Object* %76, %struct.Object** %3, align 8
  br label %195

77:                                               ; preds = %62, %59
  %78 = load %struct.Object*, %struct.Object** %4, align 8
  %79 = call zeroext i1 @object_is_float(%struct.Object* noundef %78)
  br i1 %79, label %80, label %93

80:                                               ; preds = %77
  %81 = load %struct.Object*, %struct.Object** %5, align 8
  %82 = call zeroext i1 @object_is_int(%struct.Object* noundef %81)
  br i1 %82, label %83, label %93

83:                                               ; preds = %80
  %84 = load %struct.Object*, %struct.Object** %4, align 8
  %85 = call double @object_as_float(%struct.Object* noundef %84)
  store double %85, double* %12, align 8
  %86 = load %struct.Object*, %struct.Object** %5, align 8
  %87 = call i64 @object_as_int(%struct.Object* noundef %86)
  %88 = sitofp i64 %87 to double
  store double %88, double* %13, align 8
  %89 = load double, double* %12, align 8
  %90 = load double, double* %13, align 8
  %91 = fmul double %89, %90
  %92 = call %struct.Object* @new_float(double noundef %91)
  store %struct.Object* %92, %struct.Object** %3, align 8
  br label %195

93:                                               ; preds = %80, %77
  %94 = load %struct.Object*, %struct.Object** %4, align 8
  %95 = call zeroext i1 @object_is_int(%struct.Object* noundef %94)
  br i1 %95, label %96, label %109

96:                                               ; preds = %93
  %97 = load %struct.Object*, %struct.Object** %5, align 8
  %98 = call zeroext i1 @object_is_float(%struct.Object* noundef %97)
  br i1 %98, label %99, label %109

99:                                               ; preds = %96
  %100 = load %struct.Object*, %struct.Object** %4, align 8
  %101 = call i64 @object_as_int(%struct.Object* noundef %100)
  %102 = sitofp i64 %101 to double
  store double %102, double* %14, align 8
  %103 = load %struct.Object*, %struct.Object** %5, align 8
  %104 = call double @object_as_float(%struct.Object* noundef %103)
  store double %104, double* %15, align 8
  %105 = load double, double* %14, align 8
  %106 = load double, double* %15, align 8
  %107 = fmul double %105, %106
  %108 = call %struct.Object* @new_float(double noundef %107)
  store %struct.Object* %108, %struct.Object** %3, align 8
  br label %195

109:                                              ; preds = %96, %93
  %110 = load %struct.Object*, %struct.Object** %4, align 8
  %111 = call zeroext i1 @object_is_bool(%struct.Object* noundef %110)
  br i1 %111, label %112, label %127

112:                                              ; preds = %109
  %113 = load %struct.Object*, %struct.Object** %5, align 8
  %114 = call zeroext i1 @object_is_int(%struct.Object* noundef %113)
  br i1 %114, label %115, label %127

115:                                              ; preds = %112
  %116 = load %struct.Object*, %struct.Object** %4, align 8
  %117 = call zeroext i1 @object_as_bool(%struct.Object* noundef %116)
  %118 = zext i1 %117 to i32
  store i32 %118, i32* %16, align 4
  %119 = load %struct.Object*, %struct.Object** %5, align 8
  %120 = call i64 @object_as_int(%struct.Object* noundef %119)
  %121 = trunc i64 %120 to i32
  store i32 %121, i32* %17, align 4
  %122 = load i32, i32* %16, align 4
  %123 = load i32, i32* %17, align 4
  %124 = mul nsw i32 %122, %123
  %125 = sext i32 %124 to i64
  %126 = call %struct.Object* @new_int(i64 noundef %125)
  store %struct.Object* %126, %struct.Object** %3, align 8
  br label %195

127:                                              ; preds = %112, %109
  %128 = load %struct.Object*, %struct.Object** %4, align 8
  %129 = call zeroext i1 @object_is_int(%struct.Object* noundef %128)
  br i1 %129, label %130, label %145

130:                                              ; preds = %127
  %131 = load %struct.Object*, %struct.Object** %5, align 8
  %132 = call zeroext i1 @object_is_bool(%struct.Object* noundef %131)
  br i1 %132, label %133, label %145

133:                                              ; preds = %130
  %134 = load %struct.Object*, %struct.Object** %4, align 8
  %135 = call i64 @object_as_int(%struct.Object* noundef %134)
  %136 = trunc i64 %135 to i32
  store i32 %136, i32* %18, align 4
  %137 = load %struct.Object*, %struct.Object** %5, align 8
  %138 = call zeroext i1 @object_as_bool(%struct.Object* noundef %137)
  %139 = zext i1 %138 to i32
  store i32 %139, i32* %19, align 4
  %140 = load i32, i32* %18, align 4
  %141 = load i32, i32* %19, align 4
  %142 = mul nsw i32 %140, %141
  %143 = sext i32 %142 to i64
  %144 = call %struct.Object* @new_int(i64 noundef %143)
  store %struct.Object* %144, %struct.Object** %3, align 8
  br label %195

145:                                              ; preds = %130, %127
  %146 = load %struct.Object*, %struct.Object** %4, align 8
  %147 = call zeroext i1 @object_is_bool(%struct.Object* noundef %146)
  br i1 %147, label %148, label %161

148:                                              ; preds = %145
  %149 = load %struct.Object*, %struct.Object** %5, align 8
  %150 = call zeroext i1 @object_is_float(%struct.Object* noundef %149)
  br i1 %150, label %151, label %161

151:                                              ; preds = %148
  %152 = load %struct.Object*, %struct.Object** %4, align 8
  %153 = call zeroext i1 @object_as_bool(%struct.Object* noundef %152)
  %154 = uitofp i1 %153 to double
  store double %154, double* %20, align 8
  %155 = load %struct.Object*, %struct.Object** %5, align 8
  %156 = call double @object_as_float(%struct.Object* noundef %155)
  store double %156, double* %21, align 8
  %157 = load double, double* %20, align 8
  %158 = load double, double* %21, align 8
  %159 = fmul double %157, %158
  %160 = call %struct.Object* @new_float(double noundef %159)
  store %struct.Object* %160, %struct.Object** %3, align 8
  br label %195

161:                                              ; preds = %148, %145
  %162 = load %struct.Object*, %struct.Object** %4, align 8
  %163 = call zeroext i1 @object_is_float(%struct.Object* noundef %162)
  br i1 %163, label %164, label %177

164:                                              ; preds = %161
  %165 = load %struct.Object*, %struct.Object** %5, align 8
  %166 = call zeroext i1 @object_is_bool(%struct.Object* noundef %165)
  br i1 %166, label %167, label %177

167:                                              ; preds = %164
  %168 = load %struct.Object*, %struct.Object** %4, align 8
  %169 = call double @object_as_float(%struct.Object* noundef %168)
  store double %169, double* %22, align 8
  %170 = load %struct.Object*, %struct.Object** %5, align 8
  %171 = call zeroext i1 @object_as_bool(%struct.Object* noundef %170)
  %172 = uitofp i1 %171 to double
  store double %172, double* %23, align 8
  %173 = load double, double* %22, align 8
  %174 = load double, double* %23, align 8
  %175 = fmul double %173, %174
  %176 = call %struct.Object* @new_float(double noundef %175)
  store %struct.Object* %176, %struct.Object** %3, align 8
  br label %195

177:                                              ; preds = %164, %161
  %178 = load %struct.Object*, %struct.Object** %4, align 8
  %179 = call zeroext i1 @object_is_str(%struct.Object* noundef %178)
  br i1 %179, label %180, label %194

180:                                              ; preds = %177
  %181 = load %struct.Object*, %struct.Object** %5, align 8
  %182 = call zeroext i1 @object_is_int(%struct.Object* noundef %181)
  br i1 %182, label %183, label %194

183:                                              ; preds = %180
  %184 = load %struct.Object*, %struct.Object** %4, align 8
  %185 = call i8* @object_as_str(%struct.Object* noundef %184)
  store i8* %185, i8** %24, align 8
  %186 = load %struct.Object*, %struct.Object** %5, align 8
  %187 = call i64 @object_as_int(%struct.Object* noundef %186)
  %188 = trunc i64 %187 to i32
  store i32 %188, i32* %25, align 4
  %189 = load i8*, i8** %24, align 8
  %190 = load i32, i32* %25, align 4
  %191 = call i8* @strmult(i8* noundef %189, i32 noundef %190)
  store i8* %191, i8** %26, align 8
  %192 = load i8*, i8** %26, align 8
  %193 = call %struct.Object* @new_str(i8* noundef %192)
  store %struct.Object* %193, %struct.Object** %3, align 8
  br label %195

194:                                              ; preds = %180, %177
  store %struct.Object* null, %struct.Object** %3, align 8
  br label %195

195:                                              ; preds = %194, %183, %167, %151, %133, %115, %99, %83, %65, %50, %32
  %196 = load %struct.Object*, %struct.Object** %3, align 8
  ret %struct.Object* %196
}
        "#.to_string()
    }
}
