#include <stdbool.h>
#include "object.h"
#include <assert.h>

bool str_is_truthy(const char *str)
{
    return !(strcmp(str, "") == 0);
}

/**
 * Helper to evaluate if an operand is truthy or not.
 */
bool as_truthy(Object *operand)
{
    if (operand == NULL)
    {
        return false;
    }
    switch (object_type(operand))
    {
    case Bool:
        return object_as_bool(operand);
    case Int:
    {
        bool i_as_bool = (bool)object_as_int(operand);
        return i_as_bool;
    }
    case Float:
    {
        bool f_as_bool = (bool)object_as_float(operand);
        return f_as_bool;
    }
    case Str:
    {
        const char *str = object_as_str(operand);
        return str_is_truthy(str);
    }
    default:
        // TODO: Handle invalid cases (although everything will be either truthy or falsy...)
        return false;
    }
}

bool Not(Object *operand)
{
    return !as_truthy(operand);
}

void test_not_on_str()
{
    Object *truthy_obj = new_str("Hello");
    assert(!Not(truthy_obj));
    Object *falsy_obj = new_str("");
    assert(Not(falsy_obj));
}

void test_not_on_int()
{
    Object *truthy_obj = new_int(200);
    assert(!Not(truthy_obj));
    Object *falsy_obj = new_int(0);
    assert(Not(falsy_obj));
}

void test_not_on_bool()
{
    Object *truthy_obj = new_bool(true);
    assert(!Not(truthy_obj));
    Object *falsy_obj = new_bool(false);
    assert(Not(falsy_obj));
}

void test_not_on_float()
{
    Object *truthy_obj = new_float(2.0);
    assert(!Not(truthy_obj));
    Object *falsy_obj = new_float(0.0);
    assert(Not(falsy_obj));
}

int main()
{
    test_not_on_str();
    test_not_on_int();
    test_not_on_bool();
    test_not_on_float();
    printf("Generic Not operator tests successful.\n");
    return 0;
}