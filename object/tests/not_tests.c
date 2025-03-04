#include "../Not.c"
#include <assert.h>

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

void test_not_on_list_index()
{
    List *list = create_list(sizeof(Object *), LIST_OBJ);
    Object *list_obj = new_list(list);
    object_append(list_obj, new_int(0));
    object_append(list_obj, new_int(1));
    object_append(list_obj, new_int(2));

    Object *index_0 = object_index(list_obj, new_int(0));
    Object *index_1 = object_index(list_obj, new_int(1));

    assert(Not(index_0));
    assert(!Not(index_1)); 
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
    test_not_on_list_index();
    printf("Generic Not operator tests successful.\n");
    return 0;
}