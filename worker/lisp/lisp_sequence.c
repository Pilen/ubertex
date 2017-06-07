#include "../headers.h"

LISP_BUILTIN(length, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg = NEXT(args);
    ENSURE_EMPTY(args);

    switch (arg.type) {
    case NIL:
        return VALUE_INTEGER(0);
    case STRING:
        return VALUE_INTEGER(string_length(arg.val.string_val));
    case CONS:
        return list_length(arg);
    case VECTOR:
        return VALUE_INTEGER(vector_length(arg.val.vector_val));
    case HASH:
        return VALUE_INTEGER(hash_length(arg.val.hash_val));
    default:
        return VALUE_ERROR;
    }
}

LISP_BUILTIN(nth, "") {
    ENSURE_NOT_EMPTY(args);
    Value value1 = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value value2 = NEXT(args);
    ENSURE_EMPTY(args);

    Int n;
    Value list;
    Vector *vector;

    if (value1.type == INTEGER && value2.type == CONS) {
        n = value1.val.integer_val;
        list = value2;
        if (n < 0) {
            return VALUE_ERROR;
        }
        return list_nth(list, (Unt) n);
    } else if (value1.type == CONS && value2.type == INTEGER) {
        n = value2.val.integer_val;
        list = value1;
        if (n < 0) {
            return VALUE_ERROR;
        }
        return list_nth(list, (Unt) n);
    } else if (value1.type == INTEGER && value2.type == VECTOR) {
        n = value1.val.integer_val;
        vector = value2.val.vector_val;
        if (n < 0) {
            return VALUE_ERROR;
        }
        return vector_get(vector, (Unt) n);
    } else if (value1.type == VECTOR && value2.type == INTEGER) {
        n = value2.val.integer_val;
        vector = value1.val.vector_val;
        if (n < 0) {
            return VALUE_ERROR;
        }
        return vector_get(vector, (Unt) n);
    } else {
        return VALUE_ERROR;
    }
}
