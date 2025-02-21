#include "iterator.h"

#ifndef RANGE_H
#define RANGE_H

typedef struct
{
    word start, stop, step;
    size_t length;
} Range;

extern size_t range_len(Range *range);
extern Range *create_range(word start, word stop, word step);
extern void *range_next(void *iter);
extern void print_range(Range *range);

#endif