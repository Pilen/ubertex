#include "../types.h"
#include "../symbol.h"
#include "../assert.h"
#include "../lisp.h"

LISP_BUILTIN(type_of, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }

    Value value = LIST_GET_UNSAFE(args, 1);
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
    case LIST:
        return symbols_list;
    case HASH:
        return symbols_hash;
    default:
        w_assert(false);
        return VALUE_ERROR;
    }
}
