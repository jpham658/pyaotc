#include <stdbool.h>
#include <assert.h>
#include "object.h"

bool Eq(Object *left, Object *right)
{
    if (object_is_int(left) && object_is_int(right))
    {
        int left_as_int = object_as_int(left);
        int right_as_int = object_as_int(right);
        return left_as_int == right_as_int;
    }
    else if (object_is_float(left) && object_is_float(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = object_as_float(right);
        return left_as_float == right_as_float;
    }
    else if (object_is_bool(left) && object_is_bool(right))
    {
        bool left_as_bool = object_as_bool(left);
        bool right_as_bool = object_as_bool(right);
        return left_as_bool == right_as_bool;
    }
    else if (object_is_float(left) && object_is_int(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_int(right);
        return left_as_float == right_as_float;
    }
    else if (object_is_int(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = object_as_float(right);
        return left_as_float == right_as_float;
    }
    else if (object_is_bool(left) && object_is_int(right))
    {
        bool left_as_bool = object_as_bool(left);
        int right_as_int = object_as_int(right);
        return left_as_bool == right_as_int;
    }
    else if (object_is_int(left) && object_is_bool(right))
    {
        int left_as_int = object_as_int(left);
        bool right_as_bool = object_as_bool(right);
        return left_as_int == right_as_bool;
    }
    else if (object_is_bool(left) && object_is_float(right))
    {
        bool left_as_bool = object_as_bool(left);
        double right_as_float = object_as_float(right);
        return left_as_bool == right_as_float;
    }
    else if (object_is_float(left) && object_is_bool(right))
    {
        double left_as_float = object_as_float(left);
        bool right_as_bool = object_as_bool(right);
        return left_as_float == right_as_bool;
    }
    else if (object_is_str(left) && object_is_str(right))
    {
        const char *left_as_str = object_as_str(left);
        const char *right_as_str = object_as_str(right);
        return strcmp(left_as_str, right_as_str) == 0;
    }
    else
    {
        return false;
    }
}

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

void test_incompatible_type_eq()
{
    Object *obj1 = new_str("1");
    Object *obj2 = new_int(1);
    assert(!Eq(obj1, obj2));
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
    test_incompatible_type_eq();
    printf("Generic Equal operator tests successful.\n");
    return 0;
}