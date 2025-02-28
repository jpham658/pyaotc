#include "list.h"
#include <gc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../object/object.h"
#include "range.h"

List *create_list(size_t item_size, ListElementType elt_type)
{
    List *list = (List *)GC_malloc(sizeof(List));
    if (!list)
    {
        fprintf(stderr, "Failed to allocate memory for List\n");
        exit(EXIT_FAILURE);
    }

    list->length = 0;
    list->capacity = LIST_DEFAULT_CAPACITY;
    list->item_size = item_size;
    list->elt_type = elt_type;

    list->data = GC_malloc(list->capacity * item_size);
    if (!list->data)
    {
        fprintf(stderr, "Failed to allocate memory for List data\n");
        exit(EXIT_FAILURE);
    }

    return list;
}

size_t list_len(List *list)
{
    if (list == NULL)
    {
        fprintf(stderr, "List is null.");
        exit(EXIT_FAILURE);
    }
    return list->length;
}

void *list_index(List *list, size_t index)
{
    if (index >= list->length)
    {
        fprintf(stderr, "Index out of bounds: %zu (length: %zu)\n", index, list->length);
        exit(EXIT_FAILURE);
    }
    return (uint8_t *)list->data + (index * list->item_size);
}

void *list_set(List *list, size_t index, void *value)
{
    if (index >= list->length)
    {
        fprintf(stderr, "Index out of bounds: %zu (length: %zu)\n", index, list->length);
        exit(EXIT_FAILURE);
    }
    memcpy((uint8_t *)list->data + (index * list->item_size), value, list->item_size);
}

List *list_resize(List *list)
{
    size_t new_capacity = list->capacity * LIST_RESIZE_FACTOR;
    void *new_data = GC_realloc(list->data, new_capacity * list->item_size);
    if (!new_data)
    {
        fprintf(stderr, "Failed to reallocate memory for List data\n");
        exit(EXIT_FAILURE);
    }

    list->data = new_data;
    list->capacity = new_capacity;
    return list;
}

List *list_append(List *list, void *item)
{
    if (list->length >= list->capacity)
    {
        list = list_resize(list);
        if (!list)
        {
            fprintf(stderr, "Failed to reallocate memory for List data\n");
            exit(EXIT_FAILURE);
        }
    }
    memcpy((uint8_t *)list->data + (list->length * list->item_size), item, list->item_size);
    list->length++;
    return list;
}

void *list_next(void *iter)
{
    Iterator *it = (Iterator *)iter;
    if (it->current >= it->length)
    {
        return NULL;
    }

    void *item = (char *)it->data + (it->current * it->item_size);
    it->current++;
    return item;
}

Iterator *list_iter(List *list)
{
    Iterator *iterator = create_iterator(list->data, list->item_size, list->length, list_next, ListIter);
    return iterator;
}

void print_list(List *list)
{
    if (!list)
    {
        printf("[]");
        return;
    }

    printf("[");
    for (size_t i = 0; i < list->length; i++)
    {
        void *item = list_index(list, i);
        if (!item)
        {
            printf("None");
            continue;
        }

        switch (list->elt_type)
        {
        case LIST_INT:
        {
            printf("%d", *(int *)item);
            break;
        }
        case LIST_FLOAT:
        {
            printf("%f", *(double *)item);
            break;
        }
        case LIST_STRING:
        {
            printf("\"%s\"", *(const char **)item);
            break;
        }
        case LIST_LIST:
        {
            print_list(*(List **)item);
            break;
        }
        case LIST_RANGE:
        {
            print_range(*(Range **)item);
            break;
        }
        default:
        {
            print_obj(1, *(Object **)item);
            break;
        }
        }

        if (i < list->length - 1)
        {
            printf(", ");
        }
    }
    printf("]");
}