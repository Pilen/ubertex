
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
