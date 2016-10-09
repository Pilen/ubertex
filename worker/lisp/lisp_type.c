#include "../types.h"
#include "../symbol.h"
#include "../assert.h"
#include "../lisp.h"

LISP_BUILTIN(type_of, "") {
    ENSURE_NOT_EMPTY(args);

    Value value = NEXT(args);
    if (args.type != NIL) {
        return VALUE_ERROR;
    }
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
    default:
        w_assert(false);
        return VALUE_ERROR;
    }
}
