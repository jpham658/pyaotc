/**
 * File to test generic Object class
 */
#include "../object.h"
#include <assert.h>

void test_object_is_int_on_int()
{
    Object *int_obj = new_int(2);
    assert(object_is_int(int_obj));
}

void test_object_is_int_on_non_int()
{
    Object *str_obj = new_str("Hello world!");
    assert(!object_is_int(str_obj));
}

void test_object_as_int()
{
    Object *int_obj = new_int(1);
    assert(object_as_int(int_obj) == 1);
}

void test_object_is_str_on_str()
{
    Object *str_obj = new_str("Hello world!");
    assert(object_is_str(str_obj));
}

void test_object_is_str_on_non_str()
{
    Object *float_obj = new_float(3.0);
    assert(!object_is_str(float_obj));
}

void test_object_as_str()
{
    Object *str_obj = new_str("Hello world!");
    assert(object_as_str(str_obj) == "Hello world!");
}

void test_object_is_float_on_float()
{
    Object *float_obj = new_float(2.0);
    assert(object_is_float(float_obj));
}

void test_object_is_float_on_non_float()
{
    Object *bool_obj = new_bool(true);
    assert(!object_is_float(bool_obj));
}

void test_object_as_float()
{
    Object *float_obj = new_float(2.0);
    assert(object_as_float(float_obj) == 2.0);
}

void test_object_is_bool_on_bool()
{
    Object *bool_obj = new_bool(true);
    assert(object_is_bool(bool_obj));
}

void test_object_is_bool_on_non_bool()
{
    Object *int_obj = new_int(2);
    assert(!object_is_bool(int_obj));
}

void test_object_as_bool()
{
    Object *bool_obj = new_bool(false);
    assert(object_as_bool(bool_obj) == false);
}

void test_object_is_range_on_range()
{
    Range *range = create_range(0, 2, 1);
    Object *range_obj = new_range(range);
    assert(object_is_range(range_obj));
}

void test_object_is_range_on_non_range()
{
    Object *bool_obj = new_bool(false);
    assert(!object_is_range(bool_obj));
}

void test_object_as_range()
{
    Range *range = create_range(0, 2, 1);
    Object *range_obj = new_range(range);
    assert(object_as_range(range_obj) == range);
}

void test_object_as_iterator()
{
    Range *range = create_range(0, 2, 1);
    Iterator *obj_as_iter = range_iter(range);
    Object *iter_obj = new_iterator(obj_as_iter);
    assert(object_as_iterator(iter_obj) == obj_as_iter);
}

void test_object_into_iterator()
{
    Range *range = create_range(0, 2, 1);
    Object *range_obj = new_range(range);
    Object *range_as_iter = object_into_iterator(range_obj);
    assert(object_type(range_as_iter) == IteratorT);
    Iterator *iter = object_as_iterator(range_as_iter);
    assert(iter->data == range);
}

void test_object_next()
{
    Range *range = create_range(0, 2, 1);
    Object *range_obj = new_range(range);
    Object *range_as_iter = object_into_iterator(range_obj);
    Object *next_val = object_next(range_as_iter);
    assert(object_is_int(next_val));
    assert(object_as_int(next_val) == 0);
}

void test_iterating_over_range_obj()
{
    Range *range = create_range(0, 2, 1);
    Object *range_obj = new_range(range);
    Object *range_as_iter = object_into_iterator(range_obj);
    Object *curr = object_next(range_as_iter);
    while (curr)
    {
        print_obj(1, curr);
        print_newline();
        curr = object_next(range_as_iter);
    }
}

void test_build_range_with_bool_objs()
{
    Range *range = create_range(1, 3, 1);
    Object *range_obj = new_range(range);
    Object *range_with_bools = build_range_obj(new_bool(true), new_int(3), new_int(1));
    assert(object_is_range(range_with_bools));
    Range *actual_range = object_as_range(range_with_bools);
    assert(actual_range->start == range->start);
    assert(actual_range->stop == range->stop);
    assert(actual_range->step == range->step);
}

void test_object_as_list()
{
    List *list = create_list(sizeof(Object *), LIST_OBJ);
    const char *strings[] = {"hello", "world", "python"};
    for (int i = 0; i < 3; i++)
    {
        Object *str_obj = new_str(strings[i]);
        list_append(list, &str_obj);
    }
    Object *list_obj = new_list(list);
    print_obj(1, list_obj);
    print_newline();
    List *obj_as_list = object_as_list(list_obj);
    for (int i = 0; i < 3; i++)
    {
        Object **item = (Object **)list_index(list, i);
        const char *obj_as_str = object_as_str(*item);
        assert(strcmp(obj_as_str, strings[i]) == 0);
    }
}

void test_iterating_over_list_obj()
{
    List *list = create_list(sizeof(Object *), LIST_OBJ);
    const char *strings[] = {"testing", "object", "as", "list"};
    for (int i = 0; i < 4; i++)
    {
        Object *str_obj = new_str(strings[i]);
        list_append(list, &str_obj);
    }
    Object *list_obj = new_list(list);
    print_obj(1, list_obj);
    print_newline();
    Object *list_as_iter = object_into_iterator(list_obj);
    Object *curr = object_next(list_as_iter);
    while (curr)
    {
        print_obj(1, curr);
        print_newline();
        curr = object_next(list_as_iter);
    }
}

int main()
{
    test_object_is_int_on_int();
    test_object_is_int_on_non_int();
    test_object_as_int();

    test_object_is_str_on_str();
    test_object_is_str_on_non_str();
    test_object_as_str();

    test_object_is_float_on_float();
    test_object_is_float_on_non_float();
    test_object_as_float();

    test_object_is_bool_on_bool();
    test_object_is_bool_on_non_bool();
    test_object_as_bool();

    test_object_is_range_on_range();
    test_object_is_range_on_non_range();
    test_object_as_range();

    test_object_as_iterator();
    test_object_into_iterator();
    test_object_next();

    test_iterating_over_range_obj();
    test_object_as_list();
    test_iterating_over_list_obj();

    printf("Object tests successful.\n");
    return 0;
}