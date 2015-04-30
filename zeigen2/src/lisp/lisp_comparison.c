
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"

LISP_BUILTIN(eq, "") {
    if (args -> length < 3) {
        return VALUE_ERROR;
    }

    Value head = LIST_GET_UNSAFE(args, 1);
    for (Unt i = 2; i < args -> length; i++) {
        Value other = LIST_GET_UNSAFE(args, i);
        if (!eq(head, other)) {
            return VALUE_NIL;
        }
    }

    return symbols_t;
}

LISP_BUILTIN(equal, "") {
    if (args -> length < 3) {
        return VALUE_ERROR;
    }

    Value head = LIST_GET_UNSAFE(args, 1);
    for (Unt i = 2; i < args -> length; i++) {
        Value other = LIST_GET_UNSAFE(args, i);
        if (!equal(head, other)) {
            return VALUE_NIL;
        }
    }

    return symbols_t;
}

LISP_BUILTIN(not, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }

    Value value = LIST_GET_UNSAFE(args, 1);
    switch (value.type) {
    case NIL:
        return symbols_t;
    case LIST:
        if (value.val.list_val -> length == 0) {
            return symbols_t;
        } else {
            return VALUE_NIL;
        }
    default:
        return VALUE_NIL;
    }
}
