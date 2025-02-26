#include "object.h"

char *strmult(const char *str, int n)
{
    if (n <= 0)
    {
        return "";
    }

    int len = strlen(str);
    char *result = (char *)malloc(len * n + 1);
    if (!result)
    {
        return NULL;
    }

    result[0] = '\0';
    for (int i = 0; i < n; i++)
    {
        strcat(result, str);
    }

    return result;
}

Object *Mult(Object *left, Object *right)
{
    if (left == NULL || right == NULL)
    {
        fprintf(stderr, "Incompatible types for multiplication.\n");
        exit(EXIT_FAILURE);
    }

    if (object_is_int(left) && object_is_int(right))
    {
        word left_as_int = object_as_int(left);
        word right_as_int = object_as_int(right);
        return new_int(left_as_int * right_as_int);
    }
    else if (object_is_float(left) && object_is_float(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float * right_as_float);
    }
    else if (object_is_bool(left) && object_is_bool(right))
    {
        word left_as_int = (word)object_as_bool(left);
        word right_as_int = (word)object_as_bool(right);
        return new_int(left_as_int * right_as_int);
    }
    else if (object_is_float(left) && object_is_int(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_int(right);
        return new_float(left_as_float * right_as_float);
    }
    else if (object_is_int(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float * right_as_float);
    }
    else if (object_is_bool(left) && object_is_int(right))
    {
        word left_as_int = (word)object_as_bool(left);
        word right_as_int = object_as_int(right);
        return new_int(left_as_int * right_as_int);
    }
    else if (object_is_int(left) && object_is_bool(right))
    {
        word left_as_int = object_as_int(left);
        word right_as_int = (word)object_as_bool(right);
        return new_int(left_as_int * right_as_int);
    }
    else if (object_is_bool(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_bool(left);
        double right_as_float = object_as_float(right);
        return new_float(left_as_float * right_as_float);
    }
    else if (object_is_float(left) && object_is_bool(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_bool(right);
        return new_float(left_as_float * right_as_float);
    }
    else if (object_is_str(left) && object_is_int(right))
    {
        const char *left_as_str = object_as_str(left);
        word *right_as_int = object_as_int(right);
        return new_str(strmult(left_as_str, right_as_int));
    }
    fprintf(stderr, "Incompatible types for multiplication.\n");
    exit(EXIT_FAILURE);
}