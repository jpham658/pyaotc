#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "gc/gc.h"

#ifndef ITERATOR_H
#define ITERATOR_H

typedef intptr_t word;

typedef enum
{
    RangeIter,
    ListIter,
    StringIter,
} IteratorType;

typedef struct
{
    void *data;
    size_t item_size;
    size_t length;
    size_t current;
    void *(*next)(void *);
    IteratorType data_type;
} Iterator;

extern Iterator *create_iterator(void *data, size_t item_size, size_t length,
                                 void *(*next)(void *), IteratorType data_type);

#endif