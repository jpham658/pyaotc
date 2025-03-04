#include "../string_utils.h"
#include <assert.h>

void test_str_eq() {
    assert(str_eq("hello", "hello") == true);
    assert(str_eq("hello", "world") == false);
    assert(str_eq("", "") == true);
    assert(str_eq("hello", "") == false);
    assert(str_eq("", "world") == false);
    assert(str_eq(" hello ", " hello ") == true);
    assert(str_eq(" hello", "hello ") == false);
}

void test_str_len() {
    assert(str_len("hello") == 5);
    assert(str_len("") == 0);
    assert(str_len("hello world") == 11);
    assert(str_len("!@#$%^&*()") == 10);
}

void test_str_index() {
    assert(strcmp(*str_index("hello", 0), "h") == 0);
    assert(strcmp(*str_index("hello", 1), "e") == 0);
    assert(strcmp(*str_index("hello", 4), "o") == 0);

    assert(strcmp(*str_index("hello", -1), "o") == 0);
    assert(strcmp(*str_index("hello", -2), "l") == 0);
    assert(strcmp(*str_index("hello", -5), "h") == 0);
}

void test_str_iter()
{
    const char *test_str = "Hello world!";
    Iterator *str_iterator = str_iter(test_str);
    assert(str_iterator->data == test_str);
    assert(str_iterator->current == 0);
    assert(str_iterator->data_type == StringIter);
    assert(str_iterator->item_size == sizeof(char));
    assert(str_iterator->length == 12);
}

void test_str_next()
{
    const char *test_str = "Hello world!";
    Iterator *str_iterator = str_iter(test_str);
    const char **curr = (const char **)str_next(str_iterator);
    while (curr)
    {
        printf("\"%s\"\n", *curr);
        curr = str_iterator->next(str_iterator);
    }
}

int main()
{
    GC_init();
    test_str_eq();
    test_str_len();
    test_str_index();
    test_str_iter();
    test_str_next();
    GC_gcollect();
    return 0;
}