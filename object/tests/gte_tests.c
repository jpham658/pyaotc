#include "../ObjOps.h"
#include <assert.h>

void test_int_int_gte()
{
    Object *int_obj1 = new_int(1);
    Object *int_obj2 = new_int(1);
    Object *int_obj3 = new_int(2);
    assert(GtE(int_obj1, int_obj2));
    assert(!GtE(int_obj1, int_obj3));
}

void test_float_float_gte()
{
    Object *float_obj1 = new_float(1.0);
    Object *float_obj2 = new_float(1.0);
    Object *float_obj3 = new_float(2.0);
    assert(GtE(float_obj1, float_obj2));
    assert(!GtE(float_obj1, float_obj3));
}

void test_bool_bool_gte()
{
    Object *bool_obj1 = new_bool(true);
    Object *bool_obj2 = new_bool(true);
    Object *bool_obj3 = new_bool(false);
    assert(GtE(bool_obj1, bool_obj2));
    assert(!GtE(bool_obj3, bool_obj1));
}

void test_str_str_gte()
{
    Object *str_obj1 = new_str("hi");
    Object *str_obj2 = new_str("hi");
    Object *str_obj3 = new_str("heh");
    assert(GtE(str_obj1, str_obj2));
    assert(!GtE(str_obj1, str_obj3));
}

void test_int_float_gte()
{
    Object *obj1 = new_int(1);
    Object *obj2 = new_float(0.0);
    assert(GtE(obj1, obj2));
    assert(!GtE(obj2, obj1));
}

void test_int_bool_gte()
{
    Object *obj1 = new_int(3);
    Object *obj2 = new_bool(true);
    assert(GtE(obj1, obj2));
    assert(!GtE(obj2, obj1));
}

void test_float_bool_gte()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(GtE(obj1, obj2));
    assert(!GtE(obj2, obj1));
}

int main()
{
    test_int_int_gte();
    test_float_float_gte();
    test_bool_bool_gte();
    test_str_str_gte();
    test_int_float_gte();
    test_int_bool_gte();
    test_float_bool_gte();
    printf("Generic GtE operator tests successful.\n");
    return 0;
}