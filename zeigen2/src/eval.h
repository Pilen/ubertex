#ifndef Z_EVAL_H
#define Z_EVAL_H

#include "types.h"
#include "hash.h"
#include "environment.h"
#include "lisp.h"

Value eval(Value expression, Environment *environment, List* call_stack);
Value eval_apply(Value function_symbol, Function *function, List *args, Environment *environment, List *call_stack);
void eval_bind(List *bindings, Environment *environment, List *old_bindings, List *not_bound);
void eval_unbind(Environment *environment, List *old_bindings, List *not_bound);


#endif
