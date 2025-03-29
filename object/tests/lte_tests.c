#include "../ObjOps.h"
#include <assert.h>

void test_int_int_lte()
{
    Object *int_obj1 = new_int(1);
    Object *int_obj2 = new_int(1);
    Object *int_obj3 = new_int(2);
    assert(LtE(int_obj1, int_obj2));
    assert(!LtE(int_obj3, int_obj1));
}

void test_float_float_lte()
{
    Object *float_obj1 = new_float(1.0);
    Object *float_obj2 = new_float(1.0);
    Object *float_obj3 = new_float(2.0);
    assert(LtE(float_obj1, float_obj2));
    assert(!LtE(float_obj3, float_obj1));
}

void test_bool_bool_lte()
{
    Object *bool_obj1 = new_bool(true);
    Object *bool_obj2 = new_bool(true);
    Object *bool_obj3 = new_bool(false);
    assert(LtE(bool_obj1, bool_obj2));
    assert(!LtE(bool_obj1, bool_obj3));
}

void test_str_str_lte()
{
    Object *str_obj1 = new_str("hi");
    Object *str_obj2 = new_str("hi");
    Object *str_obj3 = new_str("heh");
    assert(LtE(str_obj1, str_obj2));
    assert(!LtE(str_obj3, str_obj1));
}

void test_int_float_lte()
{
    Object *obj1 = new_int(1);
    Object *obj2 = new_float(0.0);
    assert(!LtE(obj1, obj2));
    assert(LtE(obj2, obj1));
}

void test_int_bool_lte()
{
    Object *obj1 = new_int(3);
    Object *obj2 = new_bool(true);
    assert(!LtE(obj1, obj2));
    assert(LtE(obj2, obj1));
}

void test_float_bool_lte()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(!LtE(obj1, obj2));
    assert(LtE(obj2, obj1));
}

int main()
{
    test_int_int_lte();
    test_float_float_lte();
    test_bool_bool_lte();
    test_str_str_lte();
    test_int_float_lte();
    test_int_bool_lte();
    test_float_bool_lte();
    printf("Generic LtE operator tests successful.\n");
    return 0;
}