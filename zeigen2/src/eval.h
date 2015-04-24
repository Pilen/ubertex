#ifndef Z_EVAL_H
#define Z_EVAL_H

#include "types.h"
#include "hash.h"

typedef struct {
    Hash *variables;
    Hash *functions;
} Environment;

typedef Value (*lisp_c_function) (List *args, List *call_stack);
struct Function_s {
    Bool special;
    Bool c_code;
    lisp_c_function c_function;
    List *parameters;
    Value body;
};

Value eval(Value expression, Environment *environment, List* call_stack);
Environment environment_create(void);

#endif
