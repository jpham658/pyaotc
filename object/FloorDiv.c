#include "object.h"
#include <math.h>

Object *FloorDiv(Object *left, Object *right)
{
    if (left == NULL || right == NULL)
    {
        fprintf(stderr, "Incompatible types for floor division.\n");
        exit(EXIT_FAILURE);
    }

    if (object_is_int(left) && object_is_int(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = (double)object_as_int(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_int((word)result);
    }
    else if (object_is_float(left) && object_is_float(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = object_as_float(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_float(result);
    }
    else if (object_is_bool(left) && object_is_bool(right))
    {
        double left_as_float = (double)(word)object_as_bool(left);
        double right_as_float = (double)(word)object_as_bool(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_int((word)result);
    }
    else if (object_is_float(left) && object_is_int(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_int(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_float(result);
    }
    else if (object_is_int(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = object_as_float(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_float(result);
    }
    else if (object_is_bool(left) && object_is_int(right))
    {
        double left_as_float = (double)(word)object_as_bool(left);
        double right_as_float = (double)object_as_int(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_int((word)result);
    }
    else if (object_is_int(left) && object_is_bool(right))
    {
        double left_as_float = object_as_int(left);
        double right_as_float = (double)(word)object_as_bool(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_int((word)result);
    }
    else if (object_is_bool(left) && object_is_float(right))
    {
        double left_as_float = (double)(word)object_as_bool(left);
        double right_as_float = object_as_float(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_float(result);
    }
    else if (object_is_float(left) && object_is_bool(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)(word)object_as_bool(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        double result = floor(left_as_float / right_as_float);
        return new_float(result);
    }

    fprintf(stderr, "Incompatible types for floor division.\n");
    exit(EXIT_FAILURE);
}
