/**
 * File used to define LLVM used for generic Object class
 */

#include "object.h"
#include "gc/gc.h"

bool object_is_int(Object *obj)
{
    return ((uword)obj & 0x1) == 0x1;
}

word object_as_int(Object *obj)
{
    CHECK_PREDICATE(object_is_int(obj), "Invalid int object.");
    return (word)obj >> 1;
}

Object *new_int(word val)
{
    return (Object *)(((uword)val << 1) | 0x1);
}

bool object_is_bool(Object *obj)
{
    return ((uword)obj & 0x3) == 0x0;
}

bool object_as_bool(Object *obj)
{
    CHECK_PREDICATE(object_is_bool(obj), "Invalid bool object.");
    return (bool)((uword)obj >> 3);
}

Object *new_bool(word val)
{
    return (Object *)((uword)val << 3);
}

bool object_is_heap_object(Object *obj)
{
    return ((uword)obj & 0x2) == 0x2;
}

HeapObject *object_address(Object *obj)
{
    return (HeapObject *)((uword)obj & ~0x2);
}

Object *object_from_address(HeapObject *obj)
{
    return (Object *)((uword)obj | 0x2);
}

ObjectType object_type(Object *obj)
{
    if (object_is_int(obj)) // 1
    {
        return Int;
    }
    if (object_is_bool(obj)) // 0
    {
        return Bool;
    }
    return object_address(obj)->type;
}

bool object_is_str(Object *obj)
{
    return object_type(obj) == Str;
}

bool object_is_float(Object *obj)
{
    return object_type(obj) == Float;
}

const char *object_as_str(Object *obj)
{
    CHECK_PREDICATE(object_is_str(obj), "Invalid string object.");
    return object_address(obj)->str_value;
}

double object_as_float(Object *obj)
{
    CHECK_PREDICATE(object_is_float(obj), "Invalid float object.");
    return object_address(obj)->f_value;
}

Object *new_str(const char *value)
{
    HeapObject *result = (HeapObject *)GC_malloc(sizeof *result);
    *result = (HeapObject){.type = Str, .str_value = value};
    return object_from_address(result);
}

Object *new_float(double value)
{
    HeapObject *result = (HeapObject *)GC_malloc(sizeof *result);
    *result = (HeapObject){.type = Float, .f_value = value};
    return object_from_address(result);
}

void print_int(int i)
{
    printf("%d ", i);
}

void print_bool(bool b)
{
    const char *format_str = b ? "True " : "False ";
    printf(format_str);
}

void print_str(const char *str)
{
    printf("%s ", str);
}

void print_float(double d)
{
    printf("%f ", d);
}

void print_newline()
{
    printf("\n");
}

void print_none()
{
    printf("None ");
}

void print_obj(int arg_num, Object *obj, ...)
{
    va_list args;
    va_start(args, obj);

    Object *curr = obj;

    for (int i = 0; i < arg_num; i++)
    {
        if (curr == NULL)
        {
            print_none();
            continue;
        }

        switch (object_type(curr))
        {
        case Int:
            print_int(object_as_int(curr));
            break;
        case Bool:
            print_bool(object_as_bool(curr));
            break;
        case Float:
            print_float(object_as_float(curr));
            break;
        default:
            print_str(object_as_str(curr));
            break;
        };

        curr = va_arg(args, Object *);
    }
    va_end(args);
    print_newline();
}

void print_heap_obj(HeapObject *heap_obj)
{
    Object *obj = (Object *)heap_obj;
    if (object_is_str(obj))
    {
        print_str(object_as_str(obj));
    }
    else if (object_is_float(obj))
    {
        print_float(object_as_float(obj));
    }
    else
    {
        printf("Not valid heap object.");
    }
}