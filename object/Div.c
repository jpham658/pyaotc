#include "object.h"

Object *Div(Object *left, Object *right)
{
    if (left == NULL || right == NULL)
    {
        fprintf(stderr, "Incompatible types for division.\n");
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

        return new_float(left_as_float / right_as_float);
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

        return new_float(left_as_float / right_as_float);
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

        return new_float(left_as_float / right_as_float);
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

        return new_float(left_as_float / right_as_float);
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

        return new_float(left_as_float / right_as_float);
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

        return new_float(left_as_float / right_as_float);
    }
    else if (object_is_int(left) && object_is_bool(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = (double)(word)object_as_bool(right);

        if (right_as_float == 0.0)
        {
            fprintf(stderr, "Division by zero.\n");
            exit(EXIT_FAILURE);
        }

        return new_float(left_as_float / right_as_float);
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

        return new_float(left_as_float / right_as_float);
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

        return new_float(left_as_float / right_as_float);
    }

    fprintf(stderr, "Incompatible types for division.\n");
    exit(EXIT_FAILURE);
}
