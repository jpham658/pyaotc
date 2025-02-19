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
} ObjectType;

typedef struct
{
    ObjectType type;
    union
    {
        const char *str_value;
        double f_value;
        Range *range;
    };
} HeapObject;

// Heap objects
extern bool object_is_heap_object(Object *obj);
extern HeapObject *object_address(Object *obj);
extern Object *object_from_address(HeapObject *obj);
extern ObjectType object_type(Object *obj);

extern bool object_is_str(Object *obj);
extern bool object_is_float(Object *obj);

extern const char *object_as_str(Object *obj);
extern double object_as_float(Object *obj);

extern Object *new_str(const char *value);
extern Object *new_float(double value);

// Print functions
extern void print_int(int i);
extern void print_bool(bool b);
extern void print_str(const char *str);
extern void print_range(Range *range);
extern void print_float(double d);
extern void print_newline();
extern void print_none();

extern void print_obj(int arg_num, Object *obj, ...);
extern void print_heap_obj(HeapObject *heap_obj);

#endif