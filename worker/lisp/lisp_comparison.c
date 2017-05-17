
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"

LISP_BUILTIN(eq, "") {
    ENSURE_NOT_EMPTY(args);
    Value previous = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    while (args.type == CONS) {
        Value current = NEXT(args);
        if (!eq(previous, current)) {
            return VALUE_NIL;
        }
    }
    return symbols_t;
}

LISP_BUILTIN(equal, "") {
    ENSURE_NOT_EMPTY(args);
    Value previous = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    while (args.type == CONS) {
        Value current = NEXT(args);
        if (!equal(previous, current)) {
            return VALUE_NIL;
        }
    }
    return symbols_t;
}

LISP_BUILTIN(not, "") {
    ENSURE_NOT_EMPTY(args);
    Value value = NEXT(args);
    ENSURE_EMPTY(args);
    if (value.type == NIL) {
        return symbols_t;
    } else {
        return VALUE_NIL;
    }
}


LISP_BUILTIN(greater_than, "") {
    if (args.type != CONS) {
        return VALUE_ERROR;
    }

    Value previous = NEXT(args);

    while (args.type == CONS) {
        Value current = NEXT(args);
        switch (previous.type) {
        case INTEGER:
            switch (current.type) {
            case INTEGER:
                if (previous.val.integer_val > current.val.integer_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            case FLOAT:
                if (previous.val.integer_val > current.val.float_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            default:
                return VALUE_ERROR;
            }
        case FLOAT:
            switch (current.type) {
            case INTEGER:
                if (previous.val.float_val > current.val.integer_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            case FLOAT:
                if (previous.val.float_val > current.val.float_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            default:
                return VALUE_ERROR;
            }
        default:
            return VALUE_ERROR;
        }
    }
    return symbols_t;
}
