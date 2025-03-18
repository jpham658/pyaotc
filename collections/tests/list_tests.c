#include "../list.h"
#include <assert.h>
#include <stdio.h>

void test_create_list()
{
    List *list = create_list(sizeof(int), LIST_INT);
    assert(list->length == 0);
    assert(list->capacity == LIST_DEFAULT_CAPACITY);
    assert(list->item_size == sizeof(int));
    assert(list->data != NULL);
}

void test_list_append()
{
    List *list = create_list(sizeof(int), LIST_INT);

    for (int i = 0; i < 5; i++)
    {
        list = list_append(list, &i);
        assert(list->length == i + 1);
    }

    for (int i = 0; i < 5; i++)
    {
        int *value = (int *)list_index(list, i);
        assert(*value == i);
    }
}

void test_list_index()
{
    List *list = create_list(sizeof(int), LIST_INT);

    int values[] = {10, 20, 30, 40, 50};
    for (int i = 0; i < 5; i++)
    {
        list = list_append(list, &values[i]);
        assert(list != NULL);
    }

    for (int i = 0; i < 5; i++)
    {
        int *value = (int *)list_index(list, i);
        assert(*value == values[i]);
    }
}

void test_list_set()
{
    List *list = create_list(sizeof(int), LIST_INT);

    int values[] = {10, 20, 30, 40, 50};
    for (int i = 0; i < 5; i++)
    {
        list = list_append(list, &values[i]);
    }

    int new_values[] = {100, 200, 300, 400, 500};
    for (int i = 0; i < 5; i++)
    {
        list_set(list, i, &new_values[i]);
    }

    for (int i = 0; i < 5; i++)
    {
        int *value = (int *)list_index(list, i);
        assert(*value == new_values[i]);
    }
}

void test_list_resize()
{
    List *list = create_list(sizeof(int), LIST_INT);
    assert(list->capacity == 1);

    for (int i = 0; i < 10; i++)
    {
        list = list_append(list, &i);
    }

    assert(list->length == 10);
    assert(list->capacity >= 10);

    for (int i = 0; i < 10; i++)
    {
        int *value = (int *)list_index(list, i);
        assert(*value == i);
    }
}

void test_list_iter()
{
    List *list = create_list(sizeof(int), LIST_INT);

    int values[] = {10, 20, 30, 40, 50};
    for (int i = 0; i < 5; i++)
    {
        list = list_append(list, &values[i]);
    }

    Iterator *it = list_iter(list);

    for (int i = 0; i < 5; i++)
    {
        int *value = (int *)it->next(it);
        assert(*value == values[i]);
    }
    assert(it->next(it) == NULL);

    List *string_list = create_list(sizeof(const char *), LIST_STRING);
    const char *strings[] = {"hello", "world", "python"};
    for (int i = 0; i < 3; i++)
    {
        list_append(string_list, &strings[i]);
    }
    Iterator *string_list_iter = list_iter(string_list);

    const char **curr = (const char **)string_list_iter->next(string_list_iter);
    while (curr)
    {
        printf("Iterate: \"%s\"\n", *curr);
        curr = (const char **)string_list_iter->next(string_list_iter);
    }
}

void test_list_add()
{
    List *double_list1 = create_list(sizeof(double), LIST_FLOAT);
    double values[] = {1.23, 4.56, 7.89};
    for (int i = 0; i < 3; i++)
    {
        list_append(double_list1, &values[i]);
    }

    List *double_list2 = create_list(sizeof(double), LIST_FLOAT);
    for (int i = 3; i > 0; i--)
    {
        list_append(double_list2, &values[i]);
    }

    List *expected_list = create_list(sizeof(double), LIST_FLOAT);
    for (int i = 3; i > 0; i--)
    {
        list_append(double_list2, &values[i]);
    }
    for (int i = 3; i > 0; i--)
    {
        list_append(double_list2, &values[i]);
    }

    List *actual_list = list_add(double_list1, double_list2);

    for (size_t i = 0; i < expected_list; i++)
    {
        assert(*(double *)list_index(expected_list, i) == *(double *)list_index(actual_list, i));
    }
}

void test_list_print()
{
    List *double_list = create_list(sizeof(double), LIST_FLOAT);
    double values[] = {1.23, 4.56, 7.89};
    for (int i = 0; i < 3; i++)
    {
        list_append(double_list, &values[i]);
    }
    printf("Double list: ");
    print_list(double_list); // Output: [1.230000, 4.560000, 7.890000]
    printf("\n");

    List *string_list = create_list(sizeof(const char *), LIST_STRING);
    const char *strings[] = {"hello", "world", "python"};
    for (int i = 0; i < 3; i++)
    {
        list_append(string_list, &strings[i]);
    }

    printf("String list: ");
    print_list(string_list); // Output: ["hello", "world", "python"]
    printf("\n");
}

int main()
{
    test_create_list();
    test_list_append();
    test_list_index();
    test_list_set();
    test_list_resize();
    test_list_iter();
    test_list_print();

    printf("List tests are successful.\n");
    return 0;
}