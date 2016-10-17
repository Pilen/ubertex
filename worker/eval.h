#ifndef W_EVAL_H
#define W_EVAL_H

#include "types.h"
#include "hash.h"
#include "environment.h"
#include "lisp.h"

Value eval(Value expression, Environment *environment);
Value eval_apply(Value function_symbol, Function *function, Value args, Environment *environment);
Value eval_get_bindings(Value arguments, Value parameters);

#endif
