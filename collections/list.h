#ifndef LIST_H
#define LIST_H

#include "iterator.h"
#include <stdbool.h>

typedef enum
{
    LIST_INT,
    LIST_BOOL,
    LIST_FLOAT,
    LIST_STRING,
    LIST_OBJ,
    LIST_LIST,
    LIST_RANGE,
} ListElementType;

typedef struct
{
    size_t length;
    size_t capacity;
    size_t item_size;
    void *data;
    ListElementType elt_type;
} List;

#define LIST_DEFAULT_CAPACITY 1
#define LIST_RESIZE_FACTOR 2

extern List *create_list(size_t item_size, ListElementType elt_type);
extern size_t list_len(List *list);
extern void *list_index(List *list, word index);
extern void *list_set(List *list, word index, void *value);
extern List *list_append(List *list, void *item);
extern List *list_add(List *list1, List* list2);
extern Iterator *list_iter(List *list);
extern void *list_next(void *iter);
extern void print_list(List *list);
extern List *list_resize(List *list);

#endif