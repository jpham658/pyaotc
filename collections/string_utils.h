/**
 * Helper file to define string functions
 */

#include "iterator.h"
#include "stdbool.h"
#include "string.h"

#ifndef STR_H
#define STR_H

extern size_t str_len(const char *str);
extern bool str_eq(const char *str1, const char *str2);
extern const char **str_index(const char *str, word index);
extern void *str_next(void *iter);
extern Iterator *str_iter(const char *str);
extern bool str_is_truthy(const char *str);

#endif