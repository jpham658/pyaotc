#include "object.h"

bool *Not(Object *operand)
{
    return !object_as_truthy(operand);
}