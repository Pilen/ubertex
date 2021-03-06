#include "../headers.h"

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
    ENSURE(lower < upper);
    return VALUE_INTEGER(random_int(lower, upper));
}

LISP_BUILTIN(choice, "") {
    ENSURE_NOT_EMPTY(args);
    Value choices;
    if (CDR(args).type == NIL) {
        choices = CAR(args);
    } else {
        choices = args;
    }
    ENSURE_NOT_EMPTY(choices);
    Value length_val = list_length(choices);
    w_assert(length_val.type == INTEGER);
    Unt length = length_val.val.integer_val;
    Unt choice = (Unt) random_int(0, length);
    w_assert(choice < length);
    return list_nth(choices, choice);
}
