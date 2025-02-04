use inkwell::{
    values::{FloatValue, FunctionValue, IntValue},
    AddressSpace,
};
use rustpython_parser::ast::{CmpOp, CmpOpEq};

use crate::{
    codegen::any_class_utils::{
        any_is_bool, any_is_float, any_is_int, cast_any_to_struct, get_tag, get_value,
    },
    compiler::Compiler,
};

use super::build_generic_cmp_op::BuildGenericOp;

pub fn get_eq_fn_ir() -> String {
    r#"
declare i32 @strcmp(i8* noundef, i8* noundef)

define zeroext i1 @Eq(%struct.Object* noundef %0, %struct.Object* noundef %1) #0 {
  %3 = alloca i1, align 1
  %4 = alloca %struct.Object*, align 8
  %5 = alloca %struct.Object*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca double, align 8
  %9 = alloca double, align 8
  %10 = alloca i8, align 1
  %11 = alloca i8, align 1
  %12 = alloca double, align 8
  %13 = alloca double, align 8
  %14 = alloca double, align 8
  %15 = alloca double, align 8
  %16 = alloca i8, align 1
  %17 = alloca i32, align 4
  %18 = alloca i32, align 4
  %19 = alloca i8, align 1
  %20 = alloca i8, align 1
  %21 = alloca double, align 8
  %22 = alloca double, align 8
  %23 = alloca i8, align 1
  %24 = alloca i8*, align 8
  %25 = alloca i8*, align 8
  store %struct.Object* %0, %struct.Object** %4, align 8
  store %struct.Object* %1, %struct.Object** %5, align 8
  %26 = load %struct.Object*, %struct.Object** %4, align 8
  %27 = call zeroext i1 @object_is_int(%struct.Object* noundef %26)
  br i1 %27, label %28, label %41

28:                                               ; preds = %2
  %29 = load %struct.Object*, %struct.Object** %5, align 8
  %30 = call zeroext i1 @object_is_int(%struct.Object* noundef %29)
  br i1 %30, label %31, label %41

31:                                               ; preds = %28
  %32 = load %struct.Object*, %struct.Object** %4, align 8
  %33 = call i64 @object_as_int(%struct.Object* noundef %32)
  %34 = trunc i64 %33 to i32
  store i32 %34, i32* %6, align 4
  %35 = load %struct.Object*, %struct.Object** %5, align 8
  %36 = call i64 @object_as_int(%struct.Object* noundef %35)
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %7, align 4
  %38 = load i32, i32* %6, align 4
  %39 = load i32, i32* %7, align 4
  %40 = icmp eq i32 %38, %39
  store i1 %40, i1* %3, align 1
  br label %193

41:                                               ; preds = %28, %2
  %42 = load %struct.Object*, %struct.Object** %4, align 8
  %43 = call zeroext i1 @object_is_float(%struct.Object* noundef %42)
  br i1 %43, label %44, label %55

44:                                               ; preds = %41
  %45 = load %struct.Object*, %struct.Object** %5, align 8
  %46 = call zeroext i1 @object_is_float(%struct.Object* noundef %45)
  br i1 %46, label %47, label %55

47:                                               ; preds = %44
  %48 = load %struct.Object*, %struct.Object** %4, align 8
  %49 = call double @object_as_float(%struct.Object* noundef %48)
  store double %49, double* %8, align 8
  %50 = load %struct.Object*, %struct.Object** %5, align 8
  %51 = call double @object_as_float(%struct.Object* noundef %50)
  store double %51, double* %9, align 8
  %52 = load double, double* %8, align 8
  %53 = load double, double* %9, align 8
  %54 = fcmp oeq double %52, %53
  store i1 %54, i1* %3, align 1
  br label %193

55:                                               ; preds = %44, %41
  %56 = load %struct.Object*, %struct.Object** %4, align 8
  %57 = call zeroext i1 @object_is_bool(%struct.Object* noundef %56)
  br i1 %57, label %58, label %75

58:                                               ; preds = %55
  %59 = load %struct.Object*, %struct.Object** %5, align 8
  %60 = call zeroext i1 @object_is_bool(%struct.Object* noundef %59)
  br i1 %60, label %61, label %75

61:                                               ; preds = %58
  %62 = load %struct.Object*, %struct.Object** %4, align 8
  %63 = call zeroext i1 @object_as_bool(%struct.Object* noundef %62)
  %64 = zext i1 %63 to i8
  store i8 %64, i8* %10, align 1
  %65 = load %struct.Object*, %struct.Object** %5, align 8
  %66 = call zeroext i1 @object_as_bool(%struct.Object* noundef %65)
  %67 = zext i1 %66 to i8
  store i8 %67, i8* %11, align 1
  %68 = load i8, i8* %10, align 1
  %69 = trunc i8 %68 to i1
  %70 = zext i1 %69 to i32
  %71 = load i8, i8* %11, align 1
  %72 = trunc i8 %71 to i1
  %73 = zext i1 %72 to i32
  %74 = icmp eq i32 %70, %73
  store i1 %74, i1* %3, align 1
  br label %193

75:                                               ; preds = %58, %55
  %76 = load %struct.Object*, %struct.Object** %4, align 8
  %77 = call zeroext i1 @object_is_float(%struct.Object* noundef %76)
  br i1 %77, label %78, label %90

78:                                               ; preds = %75
  %79 = load %struct.Object*, %struct.Object** %5, align 8
  %80 = call zeroext i1 @object_is_int(%struct.Object* noundef %79)
  br i1 %80, label %81, label %90

81:                                               ; preds = %78
  %82 = load %struct.Object*, %struct.Object** %4, align 8
  %83 = call double @object_as_float(%struct.Object* noundef %82)
  store double %83, double* %12, align 8
  %84 = load %struct.Object*, %struct.Object** %5, align 8
  %85 = call i64 @object_as_int(%struct.Object* noundef %84)
  %86 = sitofp i64 %85 to double
  store double %86, double* %13, align 8
  %87 = load double, double* %12, align 8
  %88 = load double, double* %13, align 8
  %89 = fcmp oeq double %87, %88
  store i1 %89, i1* %3, align 1
  br label %193

90:                                               ; preds = %78, %75
  %91 = load %struct.Object*, %struct.Object** %4, align 8
  %92 = call zeroext i1 @object_is_int(%struct.Object* noundef %91)
  br i1 %92, label %93, label %105

93:                                               ; preds = %90
  %94 = load %struct.Object*, %struct.Object** %5, align 8
  %95 = call zeroext i1 @object_is_float(%struct.Object* noundef %94)
  br i1 %95, label %96, label %105

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
  %104 = fcmp oeq double %102, %103
  store i1 %104, i1* %3, align 1
  br label %193

105:                                              ; preds = %93, %90
  %106 = load %struct.Object*, %struct.Object** %4, align 8
  %107 = call zeroext i1 @object_is_bool(%struct.Object* noundef %106)
  br i1 %107, label %108, label %123

108:                                              ; preds = %105
  %109 = load %struct.Object*, %struct.Object** %5, align 8
  %110 = call zeroext i1 @object_is_int(%struct.Object* noundef %109)
  br i1 %110, label %111, label %123

111:                                              ; preds = %108
  %112 = load %struct.Object*, %struct.Object** %4, align 8
  %113 = call zeroext i1 @object_as_bool(%struct.Object* noundef %112)
  %114 = zext i1 %113 to i8
  store i8 %114, i8* %16, align 1
  %115 = load %struct.Object*, %struct.Object** %5, align 8
  %116 = call i64 @object_as_int(%struct.Object* noundef %115)
  %117 = trunc i64 %116 to i32
  store i32 %117, i32* %17, align 4
  %118 = load i8, i8* %16, align 1
  %119 = trunc i8 %118 to i1
  %120 = zext i1 %119 to i32
  %121 = load i32, i32* %17, align 4
  %122 = icmp eq i32 %120, %121
  store i1 %122, i1* %3, align 1
  br label %193

123:                                              ; preds = %108, %105
  %124 = load %struct.Object*, %struct.Object** %4, align 8
  %125 = call zeroext i1 @object_is_int(%struct.Object* noundef %124)
  br i1 %125, label %126, label %141

126:                                              ; preds = %123
  %127 = load %struct.Object*, %struct.Object** %5, align 8
  %128 = call zeroext i1 @object_is_bool(%struct.Object* noundef %127)
  br i1 %128, label %129, label %141

129:                                              ; preds = %126
  %130 = load %struct.Object*, %struct.Object** %4, align 8
  %131 = call i64 @object_as_int(%struct.Object* noundef %130)
  %132 = trunc i64 %131 to i32
  store i32 %132, i32* %18, align 4
  %133 = load %struct.Object*, %struct.Object** %5, align 8
  %134 = call zeroext i1 @object_as_bool(%struct.Object* noundef %133)
  %135 = zext i1 %134 to i8
  store i8 %135, i8* %19, align 1
  %136 = load i32, i32* %18, align 4
  %137 = load i8, i8* %19, align 1
  %138 = trunc i8 %137 to i1
  %139 = zext i1 %138 to i32
  %140 = icmp eq i32 %136, %139
  store i1 %140, i1* %3, align 1
  br label %193

141:                                              ; preds = %126, %123
  %142 = load %struct.Object*, %struct.Object** %4, align 8
  %143 = call zeroext i1 @object_is_bool(%struct.Object* noundef %142)
  br i1 %143, label %144, label %159

144:                                              ; preds = %141
  %145 = load %struct.Object*, %struct.Object** %5, align 8
  %146 = call zeroext i1 @object_is_float(%struct.Object* noundef %145)
  br i1 %146, label %147, label %159

147:                                              ; preds = %144
  %148 = load %struct.Object*, %struct.Object** %4, align 8
  %149 = call zeroext i1 @object_as_bool(%struct.Object* noundef %148)
  %150 = zext i1 %149 to i8
  store i8 %150, i8* %20, align 1
  %151 = load %struct.Object*, %struct.Object** %5, align 8
  %152 = call double @object_as_float(%struct.Object* noundef %151)
  store double %152, double* %21, align 8
  %153 = load i8, i8* %20, align 1
  %154 = trunc i8 %153 to i1
  %155 = zext i1 %154 to i32
  %156 = sitofp i32 %155 to double
  %157 = load double, double* %21, align 8
  %158 = fcmp oeq double %156, %157
  store i1 %158, i1* %3, align 1
  br label %193

159:                                              ; preds = %144, %141
  %160 = load %struct.Object*, %struct.Object** %4, align 8
  %161 = call zeroext i1 @object_is_float(%struct.Object* noundef %160)
  br i1 %161, label %162, label %177

162:                                              ; preds = %159
  %163 = load %struct.Object*, %struct.Object** %5, align 8
  %164 = call zeroext i1 @object_is_bool(%struct.Object* noundef %163)
  br i1 %164, label %165, label %177

165:                                              ; preds = %162
  %166 = load %struct.Object*, %struct.Object** %4, align 8
  %167 = call double @object_as_float(%struct.Object* noundef %166)
  store double %167, double* %22, align 8
  %168 = load %struct.Object*, %struct.Object** %5, align 8
  %169 = call zeroext i1 @object_as_bool(%struct.Object* noundef %168)
  %170 = zext i1 %169 to i8
  store i8 %170, i8* %23, align 1
  %171 = load double, double* %22, align 8
  %172 = load i8, i8* %23, align 1
  %173 = trunc i8 %172 to i1
  %174 = zext i1 %173 to i32
  %175 = sitofp i32 %174 to double
  %176 = fcmp oeq double %171, %175
  store i1 %176, i1* %3, align 1
  br label %193

177:                                              ; preds = %162, %159
  %178 = load %struct.Object*, %struct.Object** %4, align 8
  %179 = call zeroext i1 @object_is_str(%struct.Object* noundef %178)
  br i1 %179, label %180, label %192

180:                                              ; preds = %177
  %181 = load %struct.Object*, %struct.Object** %5, align 8
  %182 = call zeroext i1 @object_is_str(%struct.Object* noundef %181)
  br i1 %182, label %183, label %192

183:                                              ; preds = %180
  %184 = load %struct.Object*, %struct.Object** %4, align 8
  %185 = call i8* @object_as_str(%struct.Object* noundef %184)
  store i8* %185, i8** %24, align 8
  %186 = load %struct.Object*, %struct.Object** %5, align 8
  %187 = call i8* @object_as_str(%struct.Object* noundef %186)
  store i8* %187, i8** %25, align 8
  %188 = load i8*, i8** %24, align 8
  %189 = load i8*, i8** %25, align 8
  %190 = call i32 @strcmp(i8* noundef %188, i8* noundef %189) #4
  %191 = icmp eq i32 %190, 0
  store i1 %191, i1* %3, align 1
  br label %193

192:                                              ; preds = %180, %177
  store i1 false, i1* %3, align 1
  br label %193

193:                                              ; preds = %192, %183, %165, %147, %129, %111, %96, %81, %61, %47, %31
  %194 = load i1, i1* %3, align 1
  ret i1 %194
}
"#.to_string()
}

impl BuildGenericOp for CmpOpEq {
    fn build_generic_op<'a>(compiler: &Compiler<'a>) -> Option<FunctionValue<'a>> {
        let main_entry = compiler.builder.get_insert_block().unwrap();
        let any_type_ptr = compiler.any_type.ptr_type(AddressSpace::default());
        let g_eq_fn_type = any_type_ptr.fn_type(&[any_type_ptr.into(), any_type_ptr.into()], false);
        let f64_type = compiler.context.f64_type();

        let fn_name = format!("{:?}", CmpOp::Eq);
        let function = compiler.module.add_function(&fn_name, g_eq_fn_type, None);

        let entry_block = compiler.context.append_basic_block(function, "entry");
        compiler.builder.position_at_end(entry_block);

        let left_any = function.get_nth_param(0).unwrap().into_pointer_value();
        let right_any = function.get_nth_param(1).unwrap().into_pointer_value();

        let left_tag_value = get_tag(left_any, compiler);
        let right_tag_value = get_tag(right_any, compiler);

        // TODO: Ask - should I just return bool here instead of Any...?
        let res_ptr = compiler
            .builder
            .build_malloc(compiler.any_type, "malloc")
            .expect("Could not allocate heap memory.");
        // Res is always boolean
        let res_tag = compiler.context.i8_type().const_int(0, false);
        let res_tag_ptr = compiler
            .builder
            .build_struct_gep(res_ptr, 0, "sum_tag_ptr")
            .expect("Error: Could not get Any container tag.");
        let _ = compiler.builder.build_store(res_tag_ptr, res_tag);
        let res_ptr_as_bool = cast_any_to_struct(res_ptr, compiler.any_bool_type, compiler);

        // Conditions
        let same_type = compiler
            .builder
            .build_int_compare(
                inkwell::IntPredicate::EQ,
                left_tag_value,
                right_tag_value,
                "",
            )
            .unwrap();

        let left_is_int = any_is_int(compiler, left_tag_value);
        let left_is_float = any_is_float(compiler, left_tag_value);
        let left_is_bool = any_is_bool(compiler, left_tag_value);
        let right_is_int = any_is_int(compiler, right_tag_value);
        let right_is_float = any_is_float(compiler, right_tag_value);
        let right_is_bool = any_is_bool(compiler, right_tag_value);

        let int_int = compiler
            .builder
            .build_and(left_is_int, right_is_int, "int_int")
            .expect("Could not build 'and' condition for int_int.");
        let float_float = compiler
            .builder
            .build_and(left_is_float, right_is_float, "float_float")
            .expect("Could not build 'and' condition for float_float.");
        let bool_bool = compiler
            .builder
            .build_and(left_is_bool, right_is_bool, "bool_bool")
            .expect("Could not build 'and' condition for bool_bool.");

        let block_same_type = compiler.context.append_basic_block(function, "same_type");
        let block_int_int = compiler.context.append_basic_block(function, "int_int");
        let block_test_float_float = compiler
            .context
            .append_basic_block(function, "test_float_float");
        let block_float_float = compiler.context.append_basic_block(function, "float_float");
        let block_test_bool_bool = compiler
            .context
            .append_basic_block(function, "test_bool_bool");
        let block_bool_bool = compiler.context.append_basic_block(function, "bool_bool");

        let block_test_left_int = compiler
            .context
            .append_basic_block(function, "test_left_int");
        let block_test_right_int = compiler
            .context
            .append_basic_block(function, "test_right_int");
        let block_left_int = compiler.context.append_basic_block(function, "left_int");
        let block_right_int = compiler.context.append_basic_block(function, "right_int");

        let block_true = compiler.context.append_basic_block(function, "true");
        let block_false = compiler.context.append_basic_block(function, "false");
        let block_merge = compiler.context.append_basic_block(function, "merge");
        let block_unreachable = compiler.context.append_basic_block(function, "unreachable");

        // Declare conditionals
        let _ = compiler.builder.build_conditional_branch(
            same_type,
            block_same_type,
            block_test_left_int,
        );

        let _ = compiler.builder.position_at_end(block_same_type);
        let _ = compiler.builder.build_conditional_branch(
            int_int,
            block_int_int,
            block_test_float_float,
        );
        let _ = compiler.builder.position_at_end(block_test_float_float);
        let _ = compiler.builder.build_conditional_branch(
            float_float,
            block_float_float,
            block_test_bool_bool,
        );
        let _ = compiler.builder.position_at_end(block_test_bool_bool);
        let _ = compiler.builder.build_conditional_branch(
            bool_bool,
            block_bool_bool,
            block_unreachable,
        );
        let _ = compiler.builder.position_at_end(block_test_left_int);
        let _ = compiler.builder.build_conditional_branch(
            right_is_float,
            block_left_int,
            block_test_right_int,
        );
        let _ = compiler.builder.position_at_end(block_test_right_int);
        let _ =
            compiler
                .builder
                .build_conditional_branch(left_is_float, block_right_int, block_false);

        // Block int_int
        let _ = compiler.builder.position_at_end(block_int_int);
        let left_as_int_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
        let right_as_int_ptr = cast_any_to_struct(right_any, compiler.any_int_type, compiler);
        let left_val = get_value(left_as_int_ptr, compiler).into_int_value();
        let right_val = get_value(right_as_int_ptr, compiler).into_int_value();
        let cmp = compiler
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, left_val, right_val, "")
            .expect("Could not compare int values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block float_float
        let _ = compiler.builder.position_at_end(block_float_float);
        let left_as_float_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
        let right_as_float_ptr = cast_any_to_struct(right_any, compiler.any_float_type, compiler);
        let left_val = get_value(left_as_float_ptr, compiler).into_float_value();
        let right_val = get_value(right_as_float_ptr, compiler).into_float_value();
        let cmp = compiler
            .builder
            .build_float_compare(inkwell::FloatPredicate::OEQ, left_val, right_val, "")
            .expect("Could not compare float values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block bool_bool
        let _ = compiler.builder.position_at_end(block_bool_bool);
        let left_as_bool_ptr = cast_any_to_struct(left_any, compiler.any_bool_type, compiler);
        let right_as_bool_ptr = cast_any_to_struct(right_any, compiler.any_bool_type, compiler);
        let left_val = get_value(left_as_bool_ptr, compiler).into_int_value();
        let right_val = get_value(right_as_bool_ptr, compiler).into_int_value();
        let cmp = compiler
            .builder
            .build_int_compare(inkwell::IntPredicate::EQ, left_val, right_val, "")
            .expect("Could not compare bool values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block left_is_int
        let _ = compiler.builder.position_at_end(block_left_int);
        let left_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
        let left_val = get_value(left_ptr, compiler).into_int_value();
        let left_as_float = compiler
            .builder
            .build_signed_int_to_float(left_val, f64_type, "")
            .expect("Could not cast left value to float.");
        let right_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
        let right_val = get_value(right_ptr, compiler).into_float_value();
        let cmp = compiler
            .builder
            .build_float_compare(inkwell::FloatPredicate::OEQ, left_as_float, right_val, "")
            .expect("Could not compare float values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block right_is_int
        let _ = compiler.builder.position_at_end(block_right_int);
        let left_ptr = cast_any_to_struct(left_any, compiler.any_float_type, compiler);
        let left_val = get_value(left_ptr, compiler).into_float_value();
        let right_ptr = cast_any_to_struct(left_any, compiler.any_int_type, compiler);
        let right_val = get_value(right_ptr, compiler).into_int_value();
        let right_as_float = compiler
            .builder
            .build_signed_int_to_float(right_val, f64_type, "")
            .expect("Could not cast right value to float.");
        let cmp = compiler
            .builder
            .build_float_compare(inkwell::FloatPredicate::OEQ, left_val, right_as_float, "")
            .expect("Could not compare float values.");
        let _ = compiler
            .builder
            .build_conditional_branch(cmp, block_true, block_false);

        // Block true
        let _ = compiler.builder.position_at_end(block_true);
        let truth_val = compiler.context.i8_type().const_int(u64::from(true), false);
        let res_val_ptr = compiler
            .builder
            .build_struct_gep(res_ptr_as_bool, 1, "res_val_ptr")
            .expect("Error: Could not get Any container value.");
        let _ = compiler.builder.build_store(res_val_ptr, truth_val);
        let _ = compiler.builder.build_unconditional_branch(block_merge);

        // Block false
        let _ = compiler.builder.position_at_end(block_false);
        let false_val = compiler
            .context
            .i8_type()
            .const_int(u64::from(false), false);
        let res_val_ptr = compiler
            .builder
            .build_struct_gep(res_ptr_as_bool, 1, "res_val_ptr")
            .expect("Error: Could not get Any container value.");
        let _ = compiler.builder.build_store(res_val_ptr, false_val);
        let _ = compiler.builder.build_unconditional_branch(block_merge);

        // Unreachable block
        let _ = compiler.builder.position_at_end(block_unreachable);
        let _ = compiler.builder.build_unreachable();

        // Merge block
        let _ = compiler.builder.position_at_end(block_merge);
        let _ = compiler.builder.build_return(Some(&res_ptr));

        // reset back to normal!
        compiler.builder.position_at_end(main_entry);

        Some(function)
    }
}
