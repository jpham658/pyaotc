#include <stdarg.h>
#include "object.h"

bool *And(int num_args, ...)
{
    va_list args;
    va_start(args, num_args);

    for (int i = 0; i < num_args; i++)
    {
        Object *arg = va_arg(args, Object *);
        if (!object_as_truthy(arg))
        {
            va_end(args);
            return false;
        }
    }

    va_end(args);
    return true;
}

bool *Or(int num_args, ...)
{
    va_list args;
    va_start(args, num_args);

    for (int i = 0; i < num_args; i++)
    {
        Object *arg = va_arg(args, Object *);
        if (object_as_truthy(arg))
        {
            va_end(args);
            return true;
        }
    }

    va_end(args);
    return false;
}
