#include "Not.c"

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