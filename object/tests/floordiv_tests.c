#include "../ObjOps.h"
#include <assert.h>

void test_int_int_floordiv()
{
    Object *obj1 = new_int(10);
    Object *obj2 = new_int(3);
    assert(object_as_int(FloorDiv(obj1, obj2)) == 3);
}

void test_bool_bool_floordiv()
{
    Object *obj1 = new_bool(false);
    Object *obj2 = new_bool(true);
    assert(object_as_int(FloorDiv(obj1, obj2)) == 0);
}

void test_float_float_floordiv()
{
    Object *obj1 = new_float(10.0);
    Object *obj2 = new_float(3.0);
    assert(object_as_float(FloorDiv(obj1, obj2)) == 3.0);
}

void test_int_float_floordiv()
{
    Object *obj1 = new_int(10);
    Object *obj2 = new_float(3);
    assert(object_as_float(FloorDiv(obj1, obj2)) == 3.0);
    assert(object_as_float(FloorDiv(obj2, obj1)) == 0.0);
}

void test_int_bool_floordiv()
{
    Object *obj1 = new_int(2);
    Object *obj2 = new_bool(true);
    assert(object_as_int(FloorDiv(obj1, obj2)) == 2);
    assert(object_as_int(FloorDiv(obj2, obj1)) == 0);
}

void test_float_bool_floordiv()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(object_as_float(FloorDiv(obj1, obj2)) == 2.0);
    assert(object_as_float(FloorDiv(obj2, obj1)) == 0.0);
}

int main()
{
    test_int_int_floordiv();
    test_int_float_floordiv();
    test_int_bool_floordiv();

    test_float_bool_floordiv();
    test_float_float_floordiv();

    test_bool_bool_floordiv();

    printf("Generic FloorDiv operator tests successful.\n");
    return 0;
}