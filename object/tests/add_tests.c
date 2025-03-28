#include "../ObjOps.h"
#include <assert.h>

void test_int_int_add()
{
    Object *obj1 = new_int(2);
    Object *obj2 = new_int(10);
    assert(false);
}

void test_bool_bool_add()
{
    Object *obj1 = new_bool(true);
    Object *obj2 = new_bool(false);
    assert(Add(obj1, obj2) == new_int(1));
}

void test_float_float_add()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_float(10.0);
    assert(object_as_float(Add(obj1, obj2)) == 12.0);
}

void test_str_str_add()
{
    Object *obj1 = new_str("Hello");
    Object *obj2 = new_str(" world!");
    const char *expected_res = "Hello world!";
    assert(strcmp(object_as_str(Add(obj1, obj2)), expected_res) == 0);
}

void test_int_float_add()
{
    Object *obj1 = new_int(2);
    Object *obj2 = new_float(10.0);
    assert(object_as_float(Add(obj1, obj2)) == 12.0);
    assert(object_as_float(Add(obj2, obj1)) == 12.0);
}

void test_int_bool_add()
{
    Object *obj1 = new_int(2);
    Object *obj2 = new_bool(true);
    assert(Add(obj1, obj2) == new_int(3));
    assert(Add(obj2, obj1) == new_int(3));
}

void test_float_bool_add()
{
    Object *obj1 = new_float(2.0);
    Object *obj2 = new_bool(true);
    assert(object_as_float(Add(obj1, obj2)) == 3.0);
    assert(object_as_float(Add(obj2, obj1)) == 3.0);
}

int main()
{
    test_int_int_add();
    test_int_float_add();
    test_int_bool_add();

    test_float_bool_add();
    test_float_float_add();

    test_bool_bool_add();

    test_str_str_add();

    printf("Generic Add operator tests successful.\n");
    return 0;
}