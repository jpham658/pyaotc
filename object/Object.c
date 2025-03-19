/**
 * File used to define LLVM used for generic Object class
 */

#include "object.h"
#include "gc/gc.h"
#include "../collections/range.h"
#include "../collections/list.h"

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

Object *new_bool(word val)
{
    return (Object *)(((uword)1 << 4) | ((uword)val << 3) | 0x0);
}

bool object_as_bool(Object *obj)
{
    CHECK_PREDICATE(object_is_bool(obj), "Invalid bool object.");
    // Extract the boolean bit
    return (bool)(((uword)obj >> 3) & 0x1);
}

bool object_is_bool(Object *obj)
{
    return obj != NULL && ((uword)obj & 0x7) == 0x0;
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

bool object_is_range(Object *obj)
{
    return object_type(obj) == RangeT;
}

bool object_is_iterator(Object *obj)
{
    return object_type(obj) == IteratorT;
}

bool object_is_list(Object *obj)
{
    return object_type(obj) == ListT;
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

Range *object_as_range(Object *obj)
{
    CHECK_PREDICATE(object_is_range(obj), "Invalid range object.");
    return object_address(obj)->range;
}

List *object_as_list(Object *obj)
{
    CHECK_PREDICATE(object_is_list(obj), "Invalid list object.");
    return object_address(obj)->list;
}

Object *new_str(const char *value)
{
    HeapObject *result = (HeapObject *)GC_malloc(sizeof *result);
    *result = (HeapObject){.type = Str, .str_value = GC_strdup(value)};
    return object_from_address(result);
}

Object *new_float(double value)
{
    HeapObject *result = (HeapObject *)GC_malloc(sizeof *result);
    *result = (HeapObject){.type = Float, .f_value = value};
    return object_from_address(result);
}

Object *new_range(Range *range)
{
    HeapObject *result = (HeapObject *)GC_malloc(sizeof *result);
    *result = (HeapObject){.type = RangeT, .range = range};
    return object_from_address(result);
}

Object *new_iterator(Iterator *iter)
{
    HeapObject *result = (HeapObject *)GC_malloc(sizeof *result);
    *result = (HeapObject){.type = IteratorT, .iter = iter};
    return object_from_address(result);
}

Object *new_list(List *list)
{
    HeapObject *result = (HeapObject *)GC_malloc(sizeof *result);
    List *list_obj = create_list(sizeof(Object *), LIST_OBJ);
    for (word i = 0; i < list->length; i++)
    {
        void *element = list_index(list, i);
        Object *boxed_element = NULL;
        switch (list->elt_type)
        {
        case LIST_INT:
        {
            word int_value = *(word *)element;
            boxed_element = new_int(int_value);
            break;
        }
        case LIST_BOOL:
        {
            bool bool_value = *(bool *)element;
            boxed_element = new_bool(bool_value);
            break;
        }
        case LIST_FLOAT:
        {
            double float_value = *(double *)element;
            boxed_element = new_float(float_value);
            break;
        }
        case LIST_STRING:
        {
            const char *str_value = *(const char **)element;
            boxed_element = new_str(str_value);
            break;
        }
        case LIST_LIST:
        {
            List *list_value = *(List **)element;
            boxed_element = new_list(list_value);
            break;
        }
        case LIST_RANGE:
        {
            Range *range_value = *(Range **)element;
            boxed_element = new_range(range_value);
            break;
        }
        default:
        {
            boxed_element = *(Object **)element;
            break;
        }
        }
        list_append(list_obj, &boxed_element);
    }
    *result = (HeapObject){.type = ListT, .list = list_obj};
    return object_from_address(result);
}

Object *build_range_obj(Object *start, Object *stop, Object *step)
{
    if ((!object_is_bool(start) && !object_is_int(start)) || (!object_is_bool(stop) && !object_is_int(stop)) || (!object_is_bool(step) && !object_is_int(step)))
    {
        fprintf(stderr, "Invalid arguments for range.\n");
        exit(EXIT_FAILURE);
    }

    word start_as_int = object_is_bool(start) ? (word *)object_as_bool(start) : object_as_int(start);
    word stop_as_int = object_is_bool(stop) ? (word *)object_as_bool(stop) : object_as_int(stop);
    word step_as_int = object_is_bool(step) ? (word *)object_as_bool(step) : object_as_int(step);

    Range *range = create_range(start_as_int, stop_as_int, step_as_int);
    return new_range(range);
}

void print_int(int i)
{
    printf("%d", i);
}

void print_bool(bool b)
{
    const char *format_str = b ? "True" : "False";
    printf(format_str);
}

void print_str(const char *str)
{
    printf("%s", str);
}

void print_float(double d)
{
    printf("%f", d);
}

void print_newline()
{
    printf("\n");
}

void print_none()
{
    printf("None");
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
            return;
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
        case RangeT:
            print_range(object_as_range(curr));
            break;
        case ListT:
            print_list(object_as_list(curr));
            break;
        case Str:
            print_str(object_as_str(curr));
            break;
        default:
            printf("invalid object\n");
        };

        if (i != arg_num - 1)
        {
            printf(" ");
        }

        curr = va_arg(args, Object *);
    }
    va_end(args);
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
    else if (object_is_range(obj))
    {
        print_range(object_as_range(obj));
    }
    else
    {
        printf("Not valid heap object.");
    }
}

bool object_is_iterable(Object *obj)
{
    if (obj == NULL)
    {
        return false;
    }

    ObjectType type = object_type(obj);

    return (type == RangeT || type == Str || type == ListT);
}

Iterator *object_as_iterator(Object *obj)
{
    CHECK_PREDICATE(object_is_iterator(obj), "Invalid iterator object.");
    return object_address(obj)->iter;
}

Object *object_into_iterator(Object *obj)
{
    CHECK_PREDICATE(object_is_iterable(obj), "Object cannot be converted to iterator.");
    switch (object_type(obj))
    {
    case RangeT:
    {
        Range *range = object_as_range(obj);
        Iterator *range_as_iter = range_iter(range);
        return new_iterator(range_as_iter);
    }
    case ListT:
    {
        List *list = object_as_list(obj);
        Iterator *list_as_iter = list_iter(list);
        return new_iterator(list_as_iter);
    }
    default:
    {
        const char *str = object_as_str(obj);
        Iterator *str_as_iter = str_iter(str);
        return new_iterator(str_as_iter);
    }
    }
}

Object *object_next(Object *obj)
{
    CHECK_PREDICATE(object_is_iterator(obj), "Cannot get next of object.");

    Iterator *obj_iter = object_as_iterator(obj);

    // case for range
    switch (obj_iter->data_type)
    {
    case RangeIter:
    {
        word *next_range_val = obj_iter->next(obj_iter);
        if (next_range_val == NULL)
        {
            return NULL;
        }
        else
        {
            return new_int(*next_range_val);
        }
        break;
    }
    case StringIter:
    {
        const char **next_str_ptr = (const char **)obj_iter->next(obj_iter);
        if (next_str_ptr == NULL)
        {
            return NULL;
        }
        else
        {
            return new_str(*next_str_ptr);
        }
    }
    default:
    {
        Object **next_obj_val = (Object **)obj_iter->next(obj_iter);
        if (next_obj_val == NULL)
        {
            return NULL;
        }
        else
        {
            return *next_obj_val;
        }
        break;
    }
    }
}

Object *object_index(Object *value, Object *slice)
{
    CHECK_PREDICATE(value != NULL, "Value is null.");
    CHECK_PREDICATE((object_is_list(value) || object_is_str(value) || object_is_range(value)), "Invalid object to be subscripted.");

    switch (object_type(value))
    {
    case ListT:
    {
        CHECK_PREDICATE(object_is_int(slice), "Cannot index with non-integer slice.");
        List *val_as_list = object_as_list(value);
        word slice_as_int = object_as_int(slice);
        Object **val_at_index = (Object **)list_index(val_as_list, slice_as_int);
        if (val_at_index == NULL)
        {
            return NULL;
        }
        return *val_at_index;
    }
    case RangeT:
    {
        CHECK_PREDICATE(object_is_int(slice), "Cannot index with non-integer slice.");
        Range *val_as_range = object_as_range(value);
        word slice_as_int = object_as_int(slice);
        word *val_at_index = (word *)range_index(val_as_range, slice_as_int);
        return new_int(*val_at_index);
    }
    default:
    {
        CHECK_PREDICATE(object_is_int(slice), "Cannot index with non-integer slice.");
        const char *val_as_str = object_as_str(value);
        word slice_as_int = object_as_int(slice);
        const char **val_at_index = str_index(val_as_str, slice_as_int);
        Object *res = new_str(*val_at_index);
        return res;
    }
    }
}

Object *object_set(Object *value, Object *slice, Object *new_value)
{
    CHECK_PREDICATE(object_is_list(value), "Cannot set object.");
    switch (object_type(value))
    {
    default:
    {
        CHECK_PREDICATE(object_is_int(slice), "Slice cannot be non-integer for list index.");
        List *val_as_list = object_as_list(value);
        word slice_as_int = object_as_int(slice);
        Object **val_at_index = (Object **)list_set(val_as_list, slice_as_int, &new_value);
        return *val_at_index;
    }
    }
}

Object *object_append(Object *object, Object *appended_val)
{
    CHECK_PREDICATE(object_is_list(object), "Cannot append to non-list.");
    List *list = object_as_list(object);
    list_append(list, &appended_val);
    return object;
}

Object *object_len(Object *object)
{
    CHECK_PREDICATE((object_is_list(object) || object_is_str(object) || object_is_range(object)), "Object has no length attribute.");
    switch (object_type(object))
    {
    case ListT:
    {
        List *list_obj = object_as_list(object);
        return new_int(list_len(list_obj));
    }
    case RangeT:
    {
        Range *range_obj = object_as_range(object);
        return new_int(range_len(range_obj));
    }
    default:
    {
        const char *str_obj = object_as_str(object);
        return new_int(str_len(str_obj));
    }
    }
}

/**
 * Helper to evaluate if an obj is truthy or not.
 */
bool object_as_truthy(Object *obj)
{
    if (obj == NULL)
    {
        return false;
    }
    switch (object_type(obj))
    {
    case Bool:
        return object_as_bool(obj);
    case Int:
    {
        bool i_as_bool = (bool)object_as_int(obj);
        return i_as_bool;
    }
    case Float:
    {
        bool f_as_bool = (bool)object_as_float(obj);
        return f_as_bool;
    }
    case Str:
    {
        const char *str = object_as_str(obj);
        return str_is_truthy(str);
    }
    case ListT:
    {
        List *list_obj = object_as_list(obj);
        return list_len(list_obj) != 0;
    }
    case RangeT:
    {
        Range *range_obj = object_as_range(obj);
        return range_len(range_obj) != 0;
    }
    default:
        // TODO: Handle invalid cases (although everything will be either truthy or falsy...)
        return false;
    }
}