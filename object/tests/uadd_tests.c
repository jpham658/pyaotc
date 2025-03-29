#include "../ObjOps.h"
#include <assert.h>

void test_int_uadd() {
    Object* obj1 = new_int(4);
    assert(object_is_int(UAdd(obj1)));
    assert(object_as_int(UAdd(obj1)) == 4);

    Object* obj2 = new_int(-4);
    assert(object_is_int(UAdd(obj2)));
    assert(object_as_int(UAdd(obj2)) == -4);

    Object* obj3 = new_int(0);
    assert(object_is_int(UAdd(obj3)));
    assert(object_as_int(UAdd(obj3)) == 0);
}

void test_float_uadd() {
    Object* obj1 = new_float(4.0);
    assert(object_is_float(UAdd(obj1)));
    assert(object_as_float(UAdd(obj1)) == 4.0);

    Object* obj2 = new_float(-4.0);
    assert(object_is_float(UAdd(obj2)));
    assert(object_as_float(UAdd(obj2)) == -4.0);

    Object* obj3 = new_float(0.0);
    assert(object_is_float(UAdd(obj3)));
    assert(object_as_float(UAdd(obj3)) == 0.0);
}

void test_bool_uadd() {
    Object *obj1 = new_bool(true);
    assert(object_is_int(UAdd(obj1)));
    assert(object_as_int(UAdd(obj1)) == 1);

    Object*obj2 = new_bool(false);
    assert(object_is_int(UAdd(obj2)));
    assert(object_as_int(UAdd(obj2)) == 0);
}

int main() {
    test_bool_uadd();
    test_int_uadd();
    test_float_uadd();
    printf("Generic UAdd operator tests successful.\n");
    return 0;
}