/**
 * File used to define LLVM used for generic Object class
 */
#ifndef OBJECT_H
#define OBJECT_H

#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include "../collections/range.h"
#include "../collections/list.h"

#define CHECK_PREDICATE(pred, msg)                                              \
    do                                                                          \
    {                                                                           \
        if (!pred)                                                              \
        {                                                                       \
            fprintf(stderr, stderr, "[Error: %s:%d] %s\n", __FILE__, __LINE__); \
            abort();                                                            \
        }                                                                       \
    } while (0)

struct Object;
typedef struct Object Object;

typedef uintptr_t uword;
typedef intptr_t word;

extern bool object_is_int(Object *obj);
extern word object_as_int(Object *obj);
extern Object *new_int(word val);

extern bool object_is_bool(Object *obj);
extern bool object_as_bool(Object *obj);
extern Object *new_bool(word val);

typedef enum
{
    Bool,
    Int,
    Str,
    Float,
    RangeT,
    IteratorT,
    ListT,
} ObjectType;

typedef struct
{
    ObjectType type;
    union
    {
        const char *str_value;
        double f_value;
        Range *range;
        Iterator *iter;
        List *list;
    };
} HeapObject;

// Heap objects
extern bool object_is_heap_object(Object *obj);
extern HeapObject *object_address(Object *obj);
extern Object *object_from_address(HeapObject *obj);
extern ObjectType object_type(Object *obj);

extern bool object_is_str(Object *obj);
extern bool object_is_float(Object *obj);
extern bool object_is_range(Object *obj);
extern bool object_is_iterator(Object *obj);
extern bool object_is_list(Object *obj);

extern const char *object_as_str(Object *obj);
extern double object_as_float(Object *obj);
extern Range *object_as_range(Object *obj);
extern List *object_as_list(Object *obj);

extern Object *new_str(const char *value);
extern Object *new_float(double value);
extern Object *new_range(Range *range);
extern Object *new_iterator(Iterator *iter);
extern Object *new_list(List *list);

// Print functions
extern void print_int(int i);
extern void print_bool(bool b);
extern void print_str(const char *str);
extern void print_float(double d);
extern void print_newline();
extern void print_none();

extern void print_obj(int arg_num, Object *obj, ...);
extern void print_heap_obj(HeapObject *heap_obj);

// Iterator functions - test if the object is iterable or not
extern bool object_is_iterable(Object *obj);
extern Iterator *object_as_iterator(Object *obj);
extern Object *object_into_iterator(Object *obj);
extern Object *build_range_obj(Object *start, Object *stop, Object *step);
extern Object *object_next(Object *obj);

#endif