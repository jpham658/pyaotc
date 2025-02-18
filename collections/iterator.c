#include "gc/gc.h"
#include "iterator.h"
#include <stdint.h>

/**
 * Method to create an iterator from any iterable object
 * e.g. lists, ranges, etc.
 *
 * @param data The actual data structure we want to iterate over
 * @param item_size The size of the items stored in data
 * @param length The length of data
 * @param next A function to move to the next item in data
 */
Iterator *create_iterator(void *data, size_t item_size, size_t length,
                          void *(*next)(void *))
{
    Iterator *iter = (Iterator *)GC_malloc(sizeof(Iterator));
    iter->data = data;
    iter->item_size = item_size;
    iter->length = length;
    iter->current = 0;
    iter->next = next;
    return iter;
}