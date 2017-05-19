#include "../math.h"
#include "../debug.h"
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../symbol.h"


LISP_BUILTIN(randint, "") {
    Int lower;
    Int upper;
    ENSURE_NOT_EMPTY(args);
    Value first = NEXT(args);
    if (first.type != INTEGER) {
        return VALUE_ERROR;
    }
    if (args.type == CONS) {
        Value second = NEXT(args);
        if (second.type != INTEGER) {
            return VALUE_ERROR;
        }
        lower = first.val.integer_val;
        upper = second.val.integer_val;
    } else {
        lower = 0;
        upper = first.val.integer_val;

    }
    ENSURE_EMPTY(args);
    if (lower >= upper) {
        return VALUE_ERROR;
    }
    return VALUE_INTEGER(random_int(lower, upper));
}
