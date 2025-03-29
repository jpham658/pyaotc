#include "../ObjOps.h"
#include <assert.h>

void test_int_int_eq()
{
    Object *int_obj1 = new_int(1);
    Object *int_obj2 = new_int(1);
    Object *int_obj3 = new_int(2);
    assert(Eq(int_obj1, int_obj2));
    assert(!Eq(int_obj1, int_obj3));
}

void test_float_float_eq()
{
    Object *float_obj1 = new_float(1.0);
    Object *float_obj2 = new_float(1.0);
    Object *float_obj3 = new_float(2.0);
    assert(Eq(float_obj1, float_obj2));
    assert(!Eq(float_obj1, float_obj3));
}

void test_bool_bool_eq()
{
    Object *bool_obj1 = new_bool(true);
    Object *bool_obj2 = new_bool(true);
    Object *bool_obj3 = new_bool(false);
    assert(Eq(bool_obj1, bool_obj2));
    assert(!Eq(bool_obj1, bool_obj3));
}

void test_str_str_eq()
{
    Object *str_obj1 = new_str("hi");
    Object *str_obj2 = new_str("hi");
    Object *str_obj3 = new_str("he");
    assert(Eq(str_obj1, str_obj2));
    assert(!Eq(str_obj1, str_obj3));
}

void test_int_float_eq()
{
    Object *obj1 = new_int(1);
    Object *obj2 = new_float(1.0);
    assert(Eq(obj1, obj2));
    assert(Eq(obj2, obj1));
    Object *obj3 = new_float(2.0);
    assert(!Eq(obj1, obj3));
}

void test_int_bool_eq()
{
    Object *obj1 = new_int(1);
    Object *obj2 = new_bool(true);
    assert(Eq(obj1, obj2));
    assert(Eq(obj2, obj1));
    Object *obj3 = new_bool(false);
    assert(!Eq(obj1, obj3));
}

void test_float_bool_eq()
{
    Object *obj1 = new_float(1.0);
    Object *obj2 = new_bool(true);
    assert(Eq(obj1, obj2));
    assert(Eq(obj2, obj1));
    Object *obj3 = new_bool(false);
    assert(!Eq(obj1, obj3));
}

int main()
{
    test_int_int_eq();
    test_float_float_eq();
    test_bool_bool_eq();
    test_str_str_eq();
    test_int_float_eq();
    test_int_bool_eq();
    test_float_bool_eq();
    printf("Generic Eq operator tests successful.\n");
    return 0;
}