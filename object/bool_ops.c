#include <stdarg.h>
#include "object.h"

bool *And(int num_args, ...)
{
    va_list args;
    va_start(args, num_args);
    Object * curr = va_arg(args, Object *);

    for (int i = 0; i < num_args; i++)
    {
        if (!object_as_truthy(curr))
        {
            va_end(args);
            return false;
        }
        curr = va_arg(args, Object *);
    }

    va_end(args);
    return true;
}

bool *Or(int num_args, Object *obj, ...)
{
    va_list args;
    va_start(args, obj);

    for (int i = 0; i < num_args; i++)
    {
        if (object_as_truthy(obj))
        {
            va_end(args);
            return true;
        }
        obj = va_arg(args, Object *);
    }

    va_end(args);
    return false;
}
