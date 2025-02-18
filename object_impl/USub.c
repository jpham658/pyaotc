#include "object.h"

Object *USub(Object *operand)
{
    if (operand == NULL)
    {
        fprintf(stderr, "Incompatible type for unary sub.\n");
        exit(EXIT_FAILURE);
    }

    switch (object_type(operand))
    {
    case Bool:
    {
        word b_as_int = (word)object_as_bool(operand);
        return new_int(-b_as_int);
    }
    case Int:
    {
        word i_as_int = object_as_int(operand);
        return new_int(-i_as_int);
    }
    case Float:
    {
        double f_as_float = object_as_float(operand);
        return new_float(-f_as_float);
    }
    default:
        fprintf(stderr, "Incompatible type for unary sub.\n");
        exit(EXIT_FAILURE);
    }
}