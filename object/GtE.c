#include "object.h"
#include "../collections/string_utils.h"
#include <string.h>

bool GtE(Object *left, Object *right)
{
    if (object_is_int(left) && object_is_int(right))
    {
        word left_as_int = object_as_int(left);
        word right_as_int = object_as_int(right);
        return left_as_int >= right_as_int;
    }
    else if (object_is_float(left) && object_is_float(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = object_as_float(right);
        return left_as_float >= right_as_float;
    }
    else if (object_is_bool(left) && object_is_bool(right))
    {
        bool left_as_bool = object_as_bool(left);
        bool right_as_bool = object_as_bool(right);
        return left_as_bool >= right_as_bool;
    }
    else if (object_is_float(left) && object_is_int(right))
    {
        double left_as_float = object_as_float(left);
        double right_as_float = (double)object_as_int(right);
        return left_as_float >= right_as_float;
    }
    else if (object_is_int(left) && object_is_float(right))
    {
        double left_as_float = (double)object_as_int(left);
        double right_as_float = object_as_float(right);
        return left_as_float >= right_as_float;
    }
    else if (object_is_bool(left) && object_is_int(right))
    {
        bool left_as_bool = object_as_bool(left);
        word right_as_int = object_as_int(right);
        return left_as_bool >= right_as_int;
    }
    else if (object_is_int(left) && object_is_bool(right))
    {
        word left_as_int = object_as_int(left);
        bool right_as_bool = object_as_bool(right);
        return left_as_int >= right_as_bool;
    }
    else if (object_is_bool(left) && object_is_float(right))
    {
        bool left_as_bool = object_as_bool(left);
        double right_as_float = object_as_float(right);
        return left_as_bool >= right_as_float;
    }
    else if (object_is_float(left) && object_is_bool(right))
    {
        double left_as_float = object_as_float(left);
        bool right_as_bool = object_as_bool(right);
        return left_as_float >= right_as_bool;
    }
    else if (object_is_str(left) && object_is_str(right))
    {
        const char *left_as_str = object_as_str(left);
        const char *right_as_str = object_as_str(right);
        return strcmp(left_as_str, right_as_str) >= 0;
    }
    else
    {
        return false;
    }
}