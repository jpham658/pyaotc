#include "../ObjOps.h"
#include <assert.h>

void test_int_int_div()
{
    Object *obj1 = new_int(10);
    Object *obj2 = new_int(2);
    assert(object_as_float(Div(obj1, obj2)) == 5.0);
}

void test_bool_bool_div()
{
    Object *obj1 = new_bool(false);
    Object *obj2 = new_bool(true);
    assert(object_as_float(Div(obj1, obj2)) == 0.0);
}

void test_float_float_div()
{
    Object *obj1 = new_float(10.0);
    Object *obj2 = new_float(2.0);
    assert(object_as_float(Div(obj1, obj2)) == 5.0);
}

void test_int_float_div()
{
    Object *obj1 = new_int(10);
    Object *obj2 = new_float(2.0);
    assert(object_as_float(Div(obj1, obj2)) == 5.0);
    assert(object_as_float(Div(obj2, obj1)) == 0.2);
}

void test_int_bool_div()
{
    Object *obj1 = new_int(2);
    Object *obj2 = new_bool(true);
    assert(object_as_float(Div(obj1, obj2)) == 2.0);
    assert(object_as_float(Div(obj2, obj1)) == 0.5);
}

void test_float_bool_div()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(object_as_float(Div(obj1, obj2)) == 2.0);
    assert(object_as_float(Div(obj2, obj1)) == 0.5);
}

int main()
{
    test_int_int_div();
    test_int_float_div();
    test_int_bool_div();

    test_float_bool_div();
    test_float_float_div();

    test_bool_bool_div();

    printf("Generic Div operator tests successful.\n");
    return 0;
}