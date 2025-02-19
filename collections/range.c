#include "range.h"

size_t range_len(Range *range)
{
    return range->length;
}

Range *create_range(int start, int stop, int step)
{
    if (step == 0)
    {
        fprintf(stderr, "Range step cannot be zero.");
        exit(EXIT_FAILURE);
    }

    Range *range = (Range *)GC_malloc(sizeof(Range));
    range->start = start;
    range->stop = stop;
    range->step = step;

    if ((step > 0 && start >= stop) || (step < 0 && start <= stop))
    {
        range->length = 0;
    }
    else
    {
        int diff = (step > 0) ? (stop - start) : (start - stop);
        range->length = (diff + abs(step) - 1) / abs(step);
    }

    return range;
}

int range_get_item(Range *range, int index, int *result)
{
    if (index < 0)
        index += range->length;
    if (index < 0 || index >= range->length)
        return 0;

    *result = range->start + index * range->step;
    return 1;
}

void *range_next(void *iter)
{
    Iterator *it = (Iterator *)iter;
    Range *range = (Range *)it->data;

    if (it->current >= it->length)
    {
        return NULL;
    }

    int value = range->start + it->current * range->step;
    it->current++;

    word *result = (word *)GC_malloc(sizeof(int));
    *result = value;
    return result;
}

Iterator *range_iter(Range *range)
{
    Iterator *iter = (Iterator *)GC_malloc(sizeof(Iterator));
    iter->data = range;
    iter->current = 0;
    iter->length = range->length;
    iter->next = range_next;
    return iter;
}

void print_range(Range *range)
{
    if (range == NULL)
    {
        printf("None");
    }
    printf("range(%d, %d, %d)", range->start, range->stop, range->step);
}