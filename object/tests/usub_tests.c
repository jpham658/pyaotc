#include "../ObjOps.h"
#include <assert.h>

void test_int_usub() {
    Object* obj1 = new_int(4);
    assert(object_is_int(USub(obj1)));
    assert(object_as_int(USub(obj1)) == -4);

    Object* obj2 = new_int(-4);
    assert(object_is_int(USub(obj2)));
    assert(object_as_int(USub(obj2)) == 4);

    Object* obj3 = new_int(0);
    assert(object_is_int(USub(obj3)));
    assert(object_as_int(USub(obj3)) == 0);
}

void test_float_usub() {
    Object* obj1 = new_float(4.0);
    assert(object_is_float(USub(obj1)));
    assert(object_as_float(USub(obj1)) == -4.0);

    Object* obj2 = new_float(-4.0);
    assert(object_is_float(USub(obj2)));
    assert(object_as_float(USub(obj2)) == 4.0);

    Object* obj3 = new_float(0.0);
    assert(object_is_float(USub(obj3)));
    assert(object_as_float(USub(obj3)) == 0.0);
}

void test_bool_usub() {
    Object *obj1 = new_bool(true);
    assert(object_is_int(USub(obj1)));
    assert(object_as_int(USub(obj1)) == -1);

    Object*obj2 = new_bool(false);
    assert(object_is_int(USub(obj2)));
    assert(object_as_int(USub(obj2)) == 0);
}

int main() {
    test_bool_usub();
    test_int_usub();
    test_float_usub();
    printf("Generic USub operator tests successful.\n");
    return 0;
}