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

void test_and_with_all_truthy()
{
    And(6, new_int(2), new_float(2.0), new_bool(true), new_str("truthy"), create_truthy_list_obj(), create_range_obj());
}

void test_and_with_one_falsy()
{
    assert(!And(6, new_int(2), new_float(2.0), new_bool(true), new_str(""), create_truthy_list_obj(), create_range_obj()));
}

void test_and_with_all_falsy()
{
    Object *falsy_list = new_list(create_list(sizeof(int), LIST_INT));
    Object *falsy_range = new_range(create_range(0, 0, 1));
    assert(!And(6, new_int(0), new_float(0.0), new_bool(false), new_str(""), falsy_list, falsy_range));
}

int main()
{
    test_and_with_all_truthy();
    test_and_with_one_falsy();
    test_and_with_all_falsy();
    printf("Generic And operator tests successful.\n");
    return 0;
}