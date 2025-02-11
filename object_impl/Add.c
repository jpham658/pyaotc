#include <stdbool.h>
#include <assert.h>
#include "object.h"

char *strconcat(const char *str1, const char *str2)
{
    size_t new_str_len = strlen(str1) + strlen(str2) + 1;
    char *new_str_ptr = (char *)malloc(new_str_len);
    strcpy(new_str_ptr, str1);
    strcat(new_str_ptr, str2);
    return new_str_ptr;
}

Object *Add(Object *left, Object *right)
{
    if (object_is_int(left) && object_is_int(right))
    {
        int left_as_int = object_as_int(left);
        int right_as_int = object_as_int(right);
        return new_int(left_as_int + right_as_int);
    }
    else if (object_is_float(left) && object_is_float(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float + right_as_float);
    }
    else if (object_is_bool(left) && object_is_bool(right))
    {
        int left_as_int = (int)object_as_bool(left);
        int right_as_int = (int)object_as_bool(right);
        return new_int(left_as_int + right_as_int);
    }
    else if (object_is_float(left) && object_is_int(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_int(right);
        return new_float(left_as_float + right_as_float);
    }
    else if (object_is_int(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float + right_as_float);
    }
    else if (object_is_bool(left) && object_is_int(right))
    {
        int left_as_int = (int)object_as_bool(left);
        int right_as_int = object_as_int(right);
        return new_int(left_as_int + right_as_int);
    }
    else if (object_is_int(left) && object_is_bool(right))
    {
        int left_as_int = object_as_int(left);
        int right_as_int = (int)object_as_bool(right);
        return new_int(left_as_int + right_as_int);
    }
    else if (object_is_bool(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_bool(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float + right_as_float);
    }
    else if (object_is_float(left) && object_is_bool(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_bool(right);
        return new_float(left_as_float + right_as_float);
    }
    else if (object_is_str(left) && object_is_str(right))
    {
        const char *left_as_str = object_as_str(left);
        const char *right_as_str = object_as_str(right);
        return new_str(strconcat(left_as_str, right_as_str));
    }
    else
    {
        return NULL;
    }
}

void test_int_int_add()
{
    Object *obj1 = new_int(2);
    Object *obj2 = new_int(10);
    assert(Add(obj1, obj2) == new_int(12));
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