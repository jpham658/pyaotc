#include "object.h"

Object *Sub(Object *left, Object *right)
{
    if (left == NULL || right == NULL)
    {
        fprintf(stderr, "Incompatible types for subtraction.\n");
        exit(EXIT_FAILURE);
    }

    if (object_is_int(left) && object_is_int(right))
    {
        word left_as_int = object_as_int(left);
        word right_as_int = object_as_int(right);
        return new_int(left_as_int - right_as_int);
    }
    else if (object_is_float(left) && object_is_float(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float - right_as_float);
    }
    else if (object_is_bool(left) && object_is_bool(right))
    {
        word left_as_int = (word)object_as_bool(left);
        word right_as_int = (word)object_as_bool(right);
        return new_int(left_as_int - right_as_int);
    }
    else if (object_is_float(left) && object_is_int(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_int(right);
        return new_float(left_as_float - right_as_float);
    }
    else if (object_is_int(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float - right_as_float);
    }
    else if (object_is_bool(left) && object_is_int(right))
    {
        word left_as_int = (word)object_as_bool(left);
        word right_as_int = object_as_int(right);
        return new_int(left_as_int - right_as_int);
    }
    else if (object_is_int(left) && object_is_bool(right))
    {
        word left_as_int = object_as_int(left);
        word right_as_int = (word)object_as_bool(right);
        return new_int(left_as_int - right_as_int);
    }
    else if (object_is_bool(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_bool(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float - right_as_float);
    }
    else if (object_is_float(left) && object_is_bool(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_bool(right);
        return new_float(left_as_float - right_as_float);
    }
    fprintf(stderr, "Incompatible types for subtraction.\n");
    exit(EXIT_FAILURE);
}