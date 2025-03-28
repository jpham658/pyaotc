/**
 * File used to define signatures of all generic operators
 */

#ifndef OBJ_OPS_H
#define OBJ_OPS_H
#include "object.h"

// Binary ops
extern Object *Add(Object *left, Object *right);
extern Object *Sub(Object *left, Object *right);
extern Object *Mult(Object *left, Object *right);

// Unary ops
extern Object *UAdd(Object *operand);
extern Object *USub(Object *operand);
extern bool *Not(Object *operand);

// Boolean ops
extern bool *And(int num_args, ...);
extern bool *Or(int num_args, ...);

// Comparison ops
extern bool Eq(Object *left, Object *right);
extern bool GtE(Object *left, Object *right);
extern bool Lt(Object *left, Object *right);

#endif