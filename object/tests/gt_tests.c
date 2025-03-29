#include "../ObjOps.h"
#include <assert.h>

void test_int_int_gt()
{
    Object *int_obj1 = new_int(3);
    Object *int_obj2 = new_int(2);
    Object *int_obj3 = new_int(4);
    assert(Gt(int_obj1, int_obj2));
    assert(!Gt(int_obj1, int_obj3));
}

void test_float_float_gt()
{
    Object *float_obj1 = new_float(3.0);
    Object *float_obj2 = new_float(2.0);
    Object *float_obj3 = new_float(4.0);
    assert(Gt(float_obj1, float_obj2));
    assert(!Gt(float_obj1, float_obj3));
}

void test_bool_bool_gt()
{
    Object *bool_obj1 = new_bool(true);
    Object *bool_obj2 = new_bool(true);
    Object *bool_obj3 = new_bool(false);
    assert(!Gt(bool_obj1, bool_obj2));
    assert(Gt(bool_obj1, bool_obj3));
}

void test_str_str_gt()
{
    Object *str_obj1 = new_str("hi");
    Object *str_obj2 = new_str("h");
    Object *str_obj3 = new_str("heh");
    assert(Gt(str_obj1, str_obj2));
    assert(!Gt(str_obj1, str_obj3));
}

void test_int_float_gt()
{
    Object *obj1 = new_int(1);
    Object *obj2 = new_float(0.0);
    assert(Gt(obj1, obj2));
    assert(!Gt(obj2, obj1));
}

void test_int_bool_gt()
{
    Object *obj1 = new_int(3);
    Object *obj2 = new_bool(true);
    assert(Gt(obj1, obj2));
    assert(!Gt(obj2, obj1));
}

void test_float_bool_gt()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(Gt(obj1, obj2));
    assert(!Gt(obj2, obj1));
}

int main()
{
    test_int_int_gt();
    test_float_float_gt();
    test_bool_bool_gt();
    test_str_str_gt();
    test_int_float_gt();
    test_int_bool_gt();
    test_float_bool_gt();
    printf("Generic Gt operator tests successful.\n");
    return 0;
}