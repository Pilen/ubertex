
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../loop.h"
#include "../assert.h"

LISP_BUILTIN(progn, "") {
    Value result = VALUE_NIL;
    while (args.type == CONS) {
        Value arg = NEXT(args);
        result = eval(arg, environment);
    }
    return result;
}

LISP_BUILTIN(quote, "") {
    ENSURE_NOT_EMPTY(args);
    Value value = NEXT(args);
    ENSURE_EMPTY(args);
    return copy_deep(value);
}

LISP_BUILTIN(eval, "") {
    ENSURE_NOT_EMPTY(args);
    Value body = NEXT(args);
    ENSURE_EMPTY(args);
    return eval(body, environment);
}

LISP_BUILTIN(if, "") {
    ENSURE_NOT_EMPTY(args);
    Value condition = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value consequent = NEXT(args);
    Value condition_result = eval(condition, environment);
    switch (condition_result.type) {
    case ERROR:
        return VALUE_ERROR;
    default:
        {
            return eval(consequent, environment);
        }
    case NIL:
        {
            Value result = VALUE_NIL;
            while (args.type == CONS) {
                Value arg = NEXT(args);
                result = eval(arg, environment);
            }
            return result;
        }
    }
}

/* NOTE: should probably be a macro */
LISP_BUILTIN(when, "") {
    ENSURE_NOT_EMPTY(args);
    Value condition = NEXT(args);
    Value condition_result = eval(condition, environment);
    switch (condition_result.type) {
    case ERROR:
        return VALUE_ERROR;
    default:
        {
            Value result = VALUE_NIL;
            while (args.type == CONS) {
                Value arg = NEXT(args);
                result = eval(arg, environment);
            }
            return result;
        }
    case NIL:
        return VALUE_NIL;
    }
}

/* NOTE: should probably be a macro */
LISP_BUILTIN(unless, "") {
    ENSURE_NOT_EMPTY(args);
    Value condition = NEXT(args);
    Value condition_result = eval(condition, environment);
    switch (condition_result.type) {
    case ERROR:
        return VALUE_ERROR;
    default:
        return VALUE_NIL;
    case NIL:
        {
            Value result = VALUE_NIL;
            while (args.type == CONS) {
                Value arg = NEXT(args);
                result = eval(arg, environment);
            }
            return result;
        }
    }
}

LISP_BUILTIN(while, "") {
    ENSURE_NOT_EMPTY(args);

    Value condition = NEXT(args);
    while (true) {
        if (loop_abort) {
            return VALUE_ERROR;
        }
        Value result = eval(condition, environment);
        switch(result.type) {
        case ERROR:
            return VALUE_ERROR;
        case NIL:
            return VALUE_NIL;
        default:
            {
                Value body = args;
                while (body.type == CONS) {
                    Value current = NEXT(body);
                    eval(current, environment);
                }
            }
        }
    }
}

LISP_BUILTIN(and, "") {
    Value result = symbols_t;
    while (args.type == CONS) {
        Value arg = NEXT(args);
        result = eval(arg, environment);
        switch(result.type) {
        case ERROR:
            return VALUE_ERROR;
        case NIL:
            return VALUE_NIL;
        default:
            continue;
        }
    }
    return result;
}

LISP_BUILTIN(or, "") {
    Value result = VALUE_NIL;
    while (args.type == CONS) {
        Value arg = NEXT(args);
        result = eval(arg, environment);
        switch(result.type) {
        case ERROR:
            return VALUE_ERROR;
        case NIL:
            continue;
        default:
            return result;
        }
    }
    return result;
}


LISP_BUILTIN(set, "") {
    ENSURE_NOT_EMPTY(args);
    Value symbol = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value value = NEXT(args);
    ENSURE_EMPTY(args);
    environment_set_variable(symbol, value, environment);
    return value;
}

LISP_BUILTIN(setq, "") {
    Value length = list_length(args);
    w_assert(length.type == INTEGER);
    if (length.val.integer_val % 2 != 0) {
        return VALUE_ERROR;
    }
    Value value = VALUE_NIL;
    while (args.type == CONS) {
        Value symbol = NEXT(args);
        Value expr = NEXT(args);
        value = eval(expr, environment);
        if (symbol.type != SYMBOL ||
            symbol.val.symbol_val == symbols_t.val.symbol_val) {
            return VALUE_ERROR;
        }
        environment_set_variable(symbol, value, environment);
    }
    return value;
}

LISP_BUILTIN(let, "") {
    ENSURE_NOT_EMPTY(args);
    Value pairs = NEXT(args);

    Value bindings = VALUE_NIL;
    while (pairs.type == CONS) {
        Value pair = NEXT(pairs);
        switch (pair.type) {
        case SYMBOL:
            bindings = CONS(CONS1(pair), bindings);
            break;
        case CONS:
            {
                Value symbol = NEXT(pair);
                Value value;
                if (pair.type == NIL) {
                    value = VALUE_NIL;
                } else if (pair.type == CONS) {
                    value = NEXT(pair);
                    ENSURE_EMPTY(pair);
                    value = eval(value, environment);
                } else {
                    return VALUE_ERROR;
                }
                bindings = CONS(CONS(symbol, value), bindings);
                break;
            }
        default:
            return VALUE_ERROR;
        }
    }
    ENSURE_EMPTY(pairs);
    environment_bind_variables(bindings, environment);
    Value result = VALUE_NIL;
    while (args.type == CONS) {
        Value body = NEXT(args);
        result = eval(body, environment);
    }
    environment_unbind_variables(environment);
    return result;
}

LISP_BUILTIN(let_star, "") {
    ENSURE_NOT_EMPTY(args);
    Value pairs = NEXT(args);
    Unt count = 0;

    while (pairs.type == CONS) {
        Value pair = NEXT(pairs);
        Value symbol;
        Value value = VALUE_NIL;
        switch (pair.type) {
        case SYMBOL:
            symbol = pair;
            break;
        case CONS:
            {
                symbol = NEXT(pair);
                if (pair.type == NIL) {
                    value = VALUE_NIL;
                } else if (pair.type == CONS) {
                    value = NEXT(pair);
                    ENSURE_EMPTY(pair);
                    value = eval(value, environment);
                } else {
                    return VALUE_ERROR;
                }
                break;
            }
        default:
            return VALUE_ERROR;
        }
        environment_bind_variables(CONS1(CONS(symbol, value)), environment);
        count++;
    }

    Value result = VALUE_NIL;
    while (args.type == CONS) {
        Value body = NEXT(args);
        result = eval(body, environment);
    }
    for (; count > 0; count--) {
        environment_unbind_variables(environment);
    }
    return result;
}

LISP_BUILTIN(print, "") {
    ENSURE_NOT_EMPTY(args);
    Value value = NEXT(args);
    ENSURE_EMPTY(args);
    print(value);
    return value;
}
