#include "ObjOps.h"

bool NotEq(Object* left, Object* right) {
    return !Eq(left, right);
}