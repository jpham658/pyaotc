#include "../ObjOps.h"
#include <assert.h>

Object *create_truthy_list_obj()
{
    List *list = create_list(sizeof(int), LIST_INT);
    for (int i = 0; i < 5; i++)
    {
        list = list_append(list, &i);
    }
    return new_list(list);
}

Object *create_range_obj()
{
    Range *range = create_range(0, 10, 2);
    return new_range(range);
}

void test_or_with_all_truthy()
{
    Or(6, new_int(2), new_float(2.0), new_bool(true), new_str("truthy"), create_truthy_list_obj(), create_range_obj());
}

void test_or_with_one_truthy()
{
    Object *falsy_list = new_list(create_list(sizeof(int), LIST_INT));
    Object *falsy_range = new_range(create_range(0, 0, 1));
    assert(Or(6, new_int(0), new_float(100.0), new_bool(false), new_str(""), falsy_list, falsy_range));
}

void test_or_with_all_falsy()
{
    Object *falsy_list = new_list(create_list(sizeof(int), LIST_INT));
    Object *falsy_range = new_range(create_range(0, 0, 1));
    assert(!Or(6, new_int(0), new_float(0.0), new_bool(false), new_str(""), falsy_list, falsy_range));
}

int main()
{
    test_or_with_all_truthy();
    test_or_with_one_truthy();
    test_or_with_all_falsy();
    printf("Generic Or operator tests successful.\n");
    return 0;
}