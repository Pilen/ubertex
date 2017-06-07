#include "../headers.h"

LISP_BUILTIN(type_of, "") {
    ENSURE_NOT_EMPTY(args);

    Value value = NEXT(args);
    ENSURE_EMPTY(args);
    switch (value.type) {
    case ERROR:
        return symbols_symbol;
    case NIL:
        return symbols_symbol;
    case INTEGER:
        return symbols_integer;
    case FLOAT:
        return symbols_float;
    case STRING:
        return symbols_string;
    case CONS:
        return symbols_cons;
    case VECTOR:
        return symbols_vector;
    case HASH:
        return symbols_hash;
    case LAMBDA:
        return symbols_lambda;
    default:
        w_assert(false);
        return VALUE_ERROR;
    }
}

LISP_BUILTIN(float, "") {
    ENSURE_NOT_EMPTY(args);
    Value value = NEXT(args);
    ENSURE_EMPTY(args);
    switch (value.type) {
    case INTEGER:
        return VALUE_FLOAT((Double) value.val.integer_val);
    case FLOAT:
        return value;
    case STRING:
        /* TODO: implement number parsing */
        w_assert(false);
    default:
        return VALUE_ERROR;
    }
}
