#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "gc/gc.h"

#ifndef ITERATOR_H
#define ITERATOR_H

typedef intptr_t word;

typedef struct
{
    void *data;
    size_t item_size;
    size_t length;
    size_t current;
    void *(*next)(void *);
} Iterator;

#endif