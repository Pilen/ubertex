#ifndef W_LISP_H
#define W_LISP_H

#include "types.h"

typedef Value (*c_lisp_function) (Value args, Environment *environment);

struct Function_s {
    Bool eval;
    Bool c_code;
    c_lisp_function c_function;
    Value parameters;
    Value body;
    String *docstring;
};

struct Lambda_s {
    Value parameters;
    Value body;
    String *docstring;
    Value lexical;
};

#endif
