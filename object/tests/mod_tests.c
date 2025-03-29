#include "../ObjOps.h"
#include <assert.h>

void test_int_int_mod()
{
    Object *obj1 = new_int(10);
    Object *obj2 = new_int(3);
    assert(object_as_int(Mod(obj1, obj2)) == 1);
}

void test_bool_bool_mod()
{
    Object *obj1 = new_bool(false);
    Object *obj2 = new_bool(true);
    assert(object_as_int(Mod(obj1, obj2)) == 0);
}

void test_float_float_mod()
{
    Object *obj1 = new_float(10.0);
    Object *obj2 = new_float(3.0);
    assert(object_as_float(Mod(obj1, obj2)) == 1.0);
}

void test_int_float_mod()
{
    Object *obj1 = new_int(10);
    Object *obj2 = new_float(3);
    assert(object_as_float(Mod(obj1, obj2)) == 1.0);
    assert(object_as_float(Mod(obj2, obj1)) == 3.0);
}

void test_int_bool_mod()
{
    Object *obj1 = new_int(2);
    Object *obj2 = new_bool(true);
    assert(object_as_int(Mod(obj1, obj2)) == 0);
    assert(object_as_int(Mod(obj2, obj1)) == 1);
}

void test_float_bool_mod()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(object_as_float(Mod(obj1, obj2)) == 0.0);
    assert(object_as_float(Mod(obj2, obj1)) == 1.0);
}

int main()
{
    test_int_int_mod();
    test_int_float_mod();
    test_int_bool_mod();

    test_float_bool_mod();
    test_float_float_mod();

    test_bool_bool_mod();

    printf("Generic Mod operator tests successful.\n");
    return 0;
}