use inkwell::{
    values::{AnyValue, FloatValue, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};
use rustpython_parser::ast::{Operator, OperatorAdd};

use crate::{codegen::generic_ops::build_generic_op::BuildGenericOp, compiler::Compiler};

/**
 * LLVM text of @Add function.
 */
fn get_gadd_fn_ir() -> String {
    r#"
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
  br i1 %bool_bool, label %handle_bool_bool, label %unreachable

handle_bool_bool:                                 ; preds = %test_bool_bool
  %40 = call i1 @object_as_bool(%struct.Object* %0)
  %41 = call i1 @object_as_bool(%struct.Object* %1)
  %42 = zext i1 %40 to i64
  %43 = zext i1 %41 to i64
  %iadd7 = add i64 %42, %43
  %44 = call %struct.Object* @new_int(i64 %iadd7)
  store %struct.Object* %44, %struct.Object** %sum, align 8
  br label %merge

unreachable:                                      ; preds = %test_bool_bool
  unreachable

merge:                                            ; preds = %handle_bool_bool, %handle_bool_float, %handle_float_bool, %handle_bool_int, %handle_int_bool, %handle_float_float, %handle_float_int, %handle_int_float, %handle_int_int
  %sum_obj = load %struct.Object*, %struct.Object** %sum, align 8
  ret %struct.Object* %sum_obj
}
"#.to_string()
}

/**
 * Generic add function that takes two Any values and returns the addition
 * of the two.
 */
impl BuildGenericOp for OperatorAdd {
    fn build_generic_op<'a>(compiler: &Compiler<'a>) -> Option<FunctionValue<'a>> {
        let main_entry = compiler.builder.get_insert_block().unwrap();

        let f64_type = compiler.context.f64_type();
        let i64_type = compiler.context.i64_type();
        let obj_ptr_type = compiler.object_type.ptr_type(AddressSpace::default());
        let g_add_fn_type =
            obj_ptr_type.fn_type(&[obj_ptr_type.into(), obj_ptr_type.into()], false);

        let fn_name = format!("{:?}", Operator::Add);
        let function = compiler.module.add_function(&fn_name, g_add_fn_type, None);

        let left_obj = function.get_nth_param(0).unwrap().into_pointer_value();
        let right_obj = function.get_nth_param(1).unwrap().into_pointer_value();

        // Get helper functions
        // These should not fail - we define them all during compiler setup.
        let object_is_int_fn = compiler.module.get_function("object_is_int").unwrap();
        let object_as_int_fn = compiler.module.get_function("object_as_int").unwrap();
        let object_is_bool_fn = compiler.module.get_function("object_is_bool").unwrap();
        let object_as_bool_fn = compiler.module.get_function("object_as_bool").unwrap();
        let object_is_float_fn = compiler.module.get_function("object_is_float").unwrap();
        let object_as_float_fn = compiler.module.get_function("object_as_float").unwrap();
        let object_is_str_fn = compiler.module.get_function("object_is_str").unwrap();
        let object_as_str_fn = compiler.module.get_function("object_as_str").unwrap();

        let entry_block = compiler.context.append_basic_block(function, "entry");
        compiler.builder.position_at_end(entry_block);
        let sum_obj_ptr = compiler
            .builder
            .build_alloca(obj_ptr_type, "sum")
            .expect("Could not allocate memory for Object pointer");

        // Define blocks
        let handle_int_int = compiler
            .context
            .append_basic_block(function, "handle_int_int");

        let test_int_float = compiler
            .context
            .append_basic_block(function, "test_int_float");
        let handle_int_float = compiler
            .context
            .append_basic_block(function, "handle_int_float");

        let test_float_int = compiler
            .context
            .append_basic_block(function, "test_float_int");
        let handle_float_int = compiler
            .context
            .append_basic_block(function, "handle_float_int");

        let test_float_float = compiler
            .context
            .append_basic_block(function, "test_float_float");
        let handle_float_float = compiler
            .context
            .append_basic_block(function, "handle_float_float");

        let test_int_bool = compiler
            .context
            .append_basic_block(function, "test_int_bool");
        let handle_int_bool = compiler
            .context
            .append_basic_block(function, "handle_int_bool");

        let test_bool_int = compiler
            .context
            .append_basic_block(function, "test_bool_int");
        let handle_bool_int = compiler
            .context
            .append_basic_block(function, "handle_bool_int");

        let test_float_bool = compiler
            .context
            .append_basic_block(function, "test_float_bool");
        let handle_float_bool = compiler
            .context
            .append_basic_block(function, "handle_float_bool");

        let test_bool_float = compiler
            .context
            .append_basic_block(function, "test_bool_float");
        let handle_bool_float = compiler
            .context
            .append_basic_block(function, "handle_bool_float");

        let test_bool_bool = compiler
            .context
            .append_basic_block(function, "test_bool_bool");
        let handle_bool_bool = compiler
            .context
            .append_basic_block(function, "handle_bool_bool");

        let unreachable_block = compiler.context.append_basic_block(function, "unreachable");
        let merge_block = compiler.context.append_basic_block(function, "merge");

        // TODO: Add string concatenation...!
        // Python can only do str + str, so you need to implement str() constructor
        let left_is_int = compiler
            .builder
            .build_call(object_is_int_fn, &[left_obj.into()], "")
            .expect("Could not test if left operand is int.")
            .as_any_value_enum()
            .into_int_value();
        let left_is_bool = compiler
            .builder
            .build_call(object_is_bool_fn, &[left_obj.into()], "")
            .expect("Could not test if left operand is bool.")
            .as_any_value_enum()
            .into_int_value();
        let left_is_float = compiler
            .builder
            .build_call(object_is_float_fn, &[left_obj.into()], "")
            .expect("Could not test if left operand is float.")
            .as_any_value_enum()
            .into_int_value();
        let left_is_str = compiler
            .builder
            .build_call(object_is_str_fn, &[left_obj.into()], "")
            .expect("Could not test if left operand is string.")
            .as_any_value_enum()
            .into_int_value();

        let right_is_int = compiler
            .builder
            .build_call(object_is_int_fn, &[right_obj.into()], "")
            .expect("Could not test if right operand is int.")
            .as_any_value_enum()
            .into_int_value();
        let right_is_bool = compiler
            .builder
            .build_call(object_is_bool_fn, &[right_obj.into()], "")
            .expect("Could not test if right operand is bool.")
            .as_any_value_enum()
            .into_int_value();
        let right_is_float = compiler
            .builder
            .build_call(object_is_float_fn, &[right_obj.into()], "")
            .expect("Could not test if right operand is float.")
            .as_any_value_enum()
            .into_int_value();
        let right_is_str = compiler
            .builder
            .build_call(object_is_str_fn, &[right_obj.into()], "")
            .expect("Could not test if right operand is string.")
            .as_any_value_enum()
            .into_int_value();

        // Define conditions
        let int_int = compiler
            .builder
            .build_and(left_is_int, right_is_int, "int_int")
            .unwrap();
        let int_float = compiler
            .builder
            .build_and(left_is_int, right_is_float, "int_float")
            .unwrap();
        let float_int = compiler
            .builder
            .build_and(left_is_float, right_is_int, "float_int")
            .unwrap();
        let float_float = compiler
            .builder
            .build_and(left_is_float, right_is_float, "float_float")
            .unwrap();
        let int_bool = compiler
            .builder
            .build_and(left_is_int, right_is_bool, "int_bool")
            .unwrap();
        let bool_int = compiler
            .builder
            .build_and(left_is_bool, right_is_int, "bool_int")
            .unwrap();
        let float_bool = compiler
            .builder
            .build_and(left_is_float, right_is_bool, "float_bool")
            .unwrap();
        let bool_float = compiler
            .builder
            .build_and(left_is_bool, right_is_float, "bool_float")
            .unwrap();
        let bool_bool = compiler
            .builder
            .build_and(left_is_bool, right_is_bool, "bool_bool")
            .unwrap();

        // Build conditions
        // entry -> handle_int_int or test_int_float
        let _ = compiler
            .builder
            .build_conditional_branch(int_int, handle_int_int, test_int_float);
        // test_int_float -> handle_int_float or test_float_int
        let _ = compiler.builder.position_at_end(test_int_float);
        let _ =
            compiler
                .builder
                .build_conditional_branch(int_float, handle_int_float, test_float_int);
        // test_float_int -> handle_float_int or test_float_float
        let _ = compiler.builder.position_at_end(test_float_int);
        let _ = compiler.builder.build_conditional_branch(
            float_int,
            handle_float_int,
            test_float_float,
        );
        // test_float_float -> handle_float_float or test_int_bool
        let _ = compiler.builder.position_at_end(test_float_float);
        let _ = compiler.builder.build_conditional_branch(
            float_float,
            handle_float_float,
            test_int_bool,
        );
        // test_int_bool -> handle_int_bool or test_bool_int
        let _ = compiler.builder.position_at_end(test_int_bool);
        let _ = compiler
            .builder
            .build_conditional_branch(int_bool, handle_int_bool, test_bool_int);
        // test_bool_int -> handle_bool_int or test_float_bool
        let _ = compiler.builder.position_at_end(test_bool_int);
        let _ =
            compiler
                .builder
                .build_conditional_branch(bool_int, handle_bool_int, test_float_bool);
        // test_float_bool -> handle_float_bool or test_bool_float
        let _ = compiler.builder.position_at_end(test_float_bool);
        let _ = compiler.builder.build_conditional_branch(
            float_bool,
            handle_float_bool,
            test_bool_float,
        );
        // test_bool_float -> handle_bool_float or test_bool_bool
        let _ = compiler.builder.position_at_end(test_bool_float);
        let _ = compiler.builder.build_conditional_branch(
            bool_float,
            handle_bool_float,
            test_bool_bool,
        );
        // test_bool_bool -> handle_bool_bool or unreachable
        let _ = compiler.builder.position_at_end(test_bool_bool);
        let _ = compiler.builder.build_conditional_branch(
            bool_bool,
            handle_bool_bool,
            unreachable_block,
        );

        // Handle blocks
        // handle_int_int
        let _ = compiler.builder.position_at_end(handle_int_int);
        let left_as_int = compiler
            .builder
            .build_call(object_as_int_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_int = compiler
            .builder
            .build_call(object_as_int_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let sum_obj = build_iadd(left_as_int, right_as_int, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_int_float
        let _ = compiler.builder.position_at_end(handle_int_float);
        let left_as_int = compiler
            .builder
            .build_call(object_as_int_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_float = compiler
            .builder
            .build_call(object_as_float_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_float_value();
        let left_as_float = compiler
            .builder
            .build_signed_int_to_float(left_as_int, f64_type, "")
            .expect("Could not cast int to float.");
        let sum_obj = build_fadd(left_as_float, right_as_float, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_float_int
        let _ = compiler.builder.position_at_end(handle_float_int);
        let left_as_float = compiler
            .builder
            .build_call(object_as_float_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_float_value();
        let right_as_int = compiler
            .builder
            .build_call(object_as_int_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_float = compiler
            .builder
            .build_signed_int_to_float(right_as_int, f64_type, "")
            .expect("Could not cast int to float.");
        let sum_obj = build_fadd(left_as_float, right_as_float, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_float_float
        let _ = compiler.builder.position_at_end(handle_float_float);
        let left_as_float = compiler
            .builder
            .build_call(object_as_float_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_float_value();
        let right_as_float = compiler
            .builder
            .build_call(object_as_float_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_float_value();
        let sum_obj = build_fadd(left_as_float, right_as_float, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_int_bool
        let _ = compiler.builder.position_at_end(handle_int_bool);
        let left_as_int = compiler
            .builder
            .build_call(object_as_int_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_bool = compiler
            .builder
            .build_call(object_as_bool_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_int = compiler
            .builder
            .build_int_z_extend(right_as_bool, i64_type, "")
            .expect("Could not cast boolean to integer.");
        let sum_obj = build_iadd(left_as_int, right_as_int, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_bool_int
        let _ = compiler.builder.position_at_end(handle_bool_int);
        let left_as_bool = compiler
            .builder
            .build_call(object_as_bool_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_int = compiler
            .builder
            .build_call(object_as_int_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let left_as_int = compiler
            .builder
            .build_int_z_extend(left_as_bool, i64_type, "")
            .expect("Could not cast boolean to integer.");
        let sum_obj = build_iadd(left_as_int, right_as_int, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_float_bool
        let _ = compiler.builder.position_at_end(handle_float_bool);
        let left_as_float = compiler
            .builder
            .build_call(object_as_float_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_float_value();
        let right_as_bool = compiler
            .builder
            .build_call(object_as_bool_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_float = compiler
            .builder
            .build_unsigned_int_to_float(right_as_bool, f64_type, "")
            .expect("Could not cast boolean to float.");
        let sum_obj = build_fadd(left_as_float, right_as_float, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_bool_float
        let _ = compiler.builder.position_at_end(handle_bool_float);
        let left_as_bool = compiler
            .builder
            .build_call(object_as_bool_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_float = compiler
            .builder
            .build_call(object_as_float_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_float_value();
        let left_as_float = compiler
            .builder
            .build_unsigned_int_to_float(left_as_bool, f64_type, "")
            .expect("Could not cast boolean to float.");
        let sum_obj = build_fadd(left_as_float, right_as_float, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // handle_bool_bool
        let _ = compiler.builder.position_at_end(handle_bool_bool);
        let left_as_bool = compiler
            .builder
            .build_call(object_as_bool_fn, &[left_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let right_as_bool = compiler
            .builder
            .build_call(object_as_bool_fn, &[right_obj.into()], "")
            .unwrap()
            .as_any_value_enum()
            .into_int_value();
        let left_as_int = compiler
            .builder
            .build_int_z_extend(left_as_bool, i64_type, "")
            .expect("Could not cast boolean to int.");
        let right_as_int = compiler
            .builder
            .build_int_z_extend(right_as_bool, i64_type, "")
            .expect("Could not cast boolean to int.");
        let sum_obj = build_iadd(left_as_int, right_as_int, compiler);
        let _ = compiler.builder.build_store(sum_obj_ptr, sum_obj);
        let _ = compiler.builder.build_unconditional_branch(merge_block);

        // merge
        let _ = compiler.builder.position_at_end(merge_block);
        let sum_obj = compiler
            .builder
            .build_load(sum_obj_ptr, "sum_obj")
            .expect("Could not load value from pointer.");
        let _ = compiler.builder.build_return(Some(&sum_obj));

        // unreachable
        let _ = compiler.builder.position_at_end(unreachable_block);
        let _ = compiler.builder.build_unreachable();

        // reset back to normal!
        compiler.builder.position_at_end(main_entry);

        Some(function)
    }
}

/**
 * Add two float values together and return a generic Object storing this float.
 */
fn build_fadd<'a>(
    left_val: FloatValue<'a>,
    right_val: FloatValue<'a>,
    compiler: &Compiler<'a>,
) -> PointerValue<'a> {
    let new_float_fn = compiler.module.get_function("new_float").unwrap();
    let float_sum_val = compiler
        .builder
        .build_float_add(left_val, right_val, &"fadd")
        .expect("Could not perform float addition.");
    let float_obj = compiler
        .builder
        .build_call(new_float_fn, &[float_sum_val.into()], "")
        .expect("Could not call new_float.")
        .as_any_value_enum();
    float_obj.into_pointer_value()
}

/**
 * Add two int values together and return a generic Object storing this int.
 */
fn build_iadd<'a>(
    left_val: IntValue<'a>,
    right_val: IntValue<'a>,
    compiler: &Compiler<'a>,
) -> PointerValue<'a> {
    let new_int_fn = compiler.module.get_function("new_int").unwrap();
    let int_sum_val = compiler
        .builder
        .build_int_add(left_val, right_val, &"iadd")
        .expect("Could not perform integer addition.");
    let int_obj = compiler
        .builder
        .build_call(new_int_fn, &[int_sum_val.into()], "")
        .expect("Could not call new_int.")
        .as_any_value_enum();
    int_obj.into_pointer_value()
}
