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
extern word range_index(Range *range, word index, word *result);
extern void *range_next(void *iter);
extern Iterator *range_iter(Range *range);
extern void print_range(Range *range);

#endif