#include "../range.c"
#include <assert.h>

void test_create_range()
{
    Range *r = create_range(2, 10, 2);
    assert(range_len(r) == 4);
    word val;
    assert(range_get_item(r, 0, &val) && val == 2);
    assert(range_get_item(r, 3, &val) && val == 8);
    Iterator *it = range_iter(r);
    assert(*(word *)it->next(it) == 2);
    assert(*(word *)it->next(it) == 4);
    assert(*(word *)it->next(it) == 6);
    assert(*(word *)it->next(it) == 8);
    assert(it->next(it) == NULL);
}

void test_create_range_with_negative_step()
{
    Range *r = create_range(10, 2, -2);
    assert(range_len(r) == 4);
    word val;
    assert(range_get_item(r, 0, &val) && val == 10);
    assert(range_get_item(r, 3, &val) && val == 4);
    Iterator *it = range_iter(r);
    assert(*(word *)it->next(it) == 10);
    assert(*(word *)it->next(it) == 8);
    assert(*(word *)it->next(it) == 6);
    assert(*(word *)it->next(it) == 4);
    assert(it->next(it) == NULL);
}

void test_create_range_with_invalid_params()
{
    Range *r1 = create_range(5, 3, 1);
    assert(range_len(r1) == 0);
    Range *r2 = create_range(3, 5, -1);
    assert(range_len(r2) == 0);
}

void test_range_get_item()
{
    Range *r = create_range(0, 5, 1);
    word val;
    assert(range_get_item(r, 0, &val) && val == 0);
    assert(range_get_item(r, 4, &val) && val == 4);
    assert(!range_get_item(r, 5, &val));
}

void test_range_get_item_with_negative_index()
{
    Range *r = create_range(2, 10, 2);
    word val;
    assert(range_get_item(r, -1, &val) && val == 8);
    assert(range_get_item(r, -2, &val) && val == 6);
    assert(!range_get_item(r, -5, &val));
}

void test_range_iter()
{
    Range *r = create_range(0, 3, 1);
    Iterator *it = range_iter(r);
    assert(*(word *)it->next(it) == 0);
    assert(*(word *)it->next(it) == 1);
    assert(*(word *)it->next(it) == 2);
    assert(it->next(it) == NULL);
}

word main()
{
    test_create_range();
    test_create_range_with_negative_step();
    test_create_range_with_invalid_params();
    test_range_get_item();
    test_range_get_item_with_negative_index();
    test_range_iter();

    Range *r = create_range(2, 10, 2);
    print_range(r);
    printf("\n");

    word value;
    if (range_get_item(r, 2, &value))
    {
        printf("Index 2: %d\n", value); // Output: 6 (2 + 2*2)
    }

    Iterator *it = range_iter(r);
    word *item = it->next(it);
    while (item)
    {
        printf("Iterate: %d\n", *item); // Outputs 2, 4, 6, 8
        item = it->next(it);
    }

    printf("Range tests are successful.\n");
    return 0;
}