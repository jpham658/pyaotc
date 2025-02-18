#include "iterator.h"

#ifndef RANGE_H
#define RANGE_H

typedef struct
{
    int start, stop, step;
    size_t length;
} Range;

extern size_t range_len(Range *range);
extern Range *create_range(int start, int stop, int step);
extern void *range_next(void *iter);

#endif