#include "../ObjOps.h"
#include <assert.h>

void test_int_int_lt()
{
    Object *int_obj1 = new_int(1);
    Object *int_obj2 = new_int(2);
    Object *int_obj3 = new_int(3);
    assert(Lt(int_obj1, int_obj2));
    assert(!Lt(int_obj3, int_obj1));
}

void test_float_float_lt()
{
    Object *float_obj1 = new_float(1.0);
    Object *float_obj2 = new_float(2.0);
    Object *float_obj3 = new_float(3.0);
    assert(Lt(float_obj1, float_obj2));
    assert(!Lt(float_obj3, float_obj1));
}

void test_bool_bool_lt()
{
    Object *bool_obj1 = new_bool(false);
    Object *bool_obj2 = new_bool(true);
    Object *bool_obj3 = new_bool(false);
    assert(Lt(bool_obj1, bool_obj2));
    assert(!Lt(bool_obj1, bool_obj3));
}

void test_str_str_lt()
{
    Object *str_obj1 = new_str("hi");
    Object *str_obj2 = new_str("hih");
    assert(Lt(str_obj1, str_obj2));
    assert(!Lt(str_obj2, str_obj1));
}

void test_int_float_lt()
{
    Object *obj1 = new_int(1);
    Object *obj2 = new_float(0.0);
    assert(!Lt(obj1, obj2));
    assert(Lt(obj2, obj1));
}

void test_int_bool_lt()
{
    Object *obj1 = new_int(3);
    Object *obj2 = new_bool(true);
    assert(!Lt(obj1, obj2));
    assert(Lt(obj2, obj1));
}

void test_float_bool_lt()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(!Lt(obj1, obj2));
    assert(Lt(obj2, obj1));
}

int main()
{
    test_int_int_lt();
    test_float_float_lt();
    test_bool_bool_lt();
    test_str_str_lt();
    test_int_float_lt();
    test_int_bool_lt();
    test_float_bool_lt();
    printf("Generic Lt operator tests successful.\n");
    return 0;
}