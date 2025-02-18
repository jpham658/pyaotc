/**
 * File to test generic Object class
 */
#include "../object.h"
#include <assert.h>

void test_object_is_int_on_int()
{
    Object *int_obj = new_int(2);
    assert(object_is_int(int_obj));
}

void test_object_is_int_on_non_int()
{
    Object *str_obj = new_str("Hello world!");
    assert(!object_is_int(str_obj));
}

void test_object_as_int()
{
    Object *int_obj = new_int(1);
    assert(object_as_int(int_obj) == 1);
}

void test_object_is_str_on_str()
{
    Object *str_obj = new_str("Hello world!");
    assert(object_is_str(str_obj));
}

void test_object_is_str_on_non_str()
{
    Object *float_obj = new_float(3.0);
    assert(!object_is_str(float_obj));
}

void test_object_as_str()
{
    Object *str_obj = new_str("Hello world!");
    assert(object_as_str(str_obj) == "Hello world!");
}

void test_object_is_float_on_float()
{
    Object *float_obj = new_float(2.0);
    assert(object_is_float(float_obj));
}

void test_object_is_float_on_non_float()
{
    Object *bool_obj = new_bool(true);
    assert(!object_is_float(bool_obj));
}

void test_object_as_float()
{
    Object *float_obj = new_float(2.0);
    assert(object_as_float(float_obj) == 2.0);
}

void test_object_is_bool_on_bool()
{
    Object *bool_obj = new_bool(true);
    assert(object_is_bool(bool_obj));
}

void test_object_is_bool_on_non_bool()
{
    Object *int_obj = new_int(2);
    assert(!object_is_bool(int_obj));
}

void test_object_as_bool()
{
    Object *bool_obj = new_bool(false);
    assert(object_as_bool(bool_obj) == false);
}

int main()
{
    test_object_is_int_on_int();
    test_object_is_int_on_non_int();
    test_object_as_int();

    test_object_is_str_on_str();
    test_object_is_str_on_non_str();
    test_object_as_str();

    test_object_is_float_on_float();
    test_object_is_float_on_non_float();
    test_object_as_float();

    test_object_is_bool_on_bool();
    test_object_is_bool_on_non_bool();
    test_object_as_bool();

    printf("Object tests successful.\n");
    return 0;
}