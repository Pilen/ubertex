
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"

LISP_BUILTIN(progn, "") {
    Value value = VALUE_NIL;
    for (Unt i = 1; i < args -> length; i++) {
        Value arg = LIST_GET_UNSAFE(args, i);
        value = eval(arg, environment, call_stack);
    }
    return value;
}

LISP_BUILTIN(quote, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }
    Value value = LIST_GET_UNSAFE(args, 1);
    return copy_deep(value);
}

LISP_BUILTIN(eval, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }

    Value body = LIST_GET_UNSAFE(args, 1);
    return eval(body, environment, call_stack);
}

LISP_BUILTIN(list, "") {
    if (args -> length == 1) {
        return VALUE_NIL;
    }
    List *result = list_create(round_up_to_power_of_2(args -> length - 1));
    for (Unt i = 1; i < args -> length; i++) {
        /* Already evaluated */
        Value value = LIST_GET_UNSAFE(args, i);
        list_push_back(result, value);
    }
    return VALUE_LIST(result);
}

LISP_BUILTIN(if, "") {
    if (args -> length < 3) {
        return VALUE_ERROR;
    }

    Value condition = eval(LIST_GET_UNSAFE(args, 1), environment, call_stack);
    condition = NILIFY(condition);
    switch (condition.type) {
    case ERROR:
        return VALUE_ERROR;
    default:
        {
            Value consequent = eval(LIST_GET_UNSAFE(args, 2), environment, call_stack);
            return consequent;
        }
    case NIL:
        {
            Value alternative = VALUE_NIL;
            for (Unt i = 3; i < args -> length; i++) {
                alternative = eval(LIST_GET_UNSAFE(args, i), environment, call_stack);
            }
            return alternative;
        }
    }
}

/* NOTE: should probably be a macro */
LISP_BUILTIN(when, "") {
    if (args -> length < 2) {
        return VALUE_ERROR;
    }
    Value condition = eval(LIST_GET_UNSAFE(args, 1), environment, call_stack);
    condition = NILIFY(condition);
    switch (condition.type) {
    case ERROR:
        return VALUE_ERROR;
    default:
        {
            Value alternative = VALUE_NIL;
            for (Unt i = 2; i < args -> length; i++) {
                alternative = eval(LIST_GET_UNSAFE(args, i), environment, call_stack);
            }
            return alternative;
        }
    case NIL:
        return VALUE_NIL;
    }
}

/* NOTE: should probably be a macro */
LISP_BUILTIN(unless, "") {
    if (args -> length < 2) {
        return VALUE_ERROR;
    }
    Value condition = eval(LIST_GET_UNSAFE(args, 1), environment, call_stack);
    condition = NILIFY(condition);
    switch (condition.type) {
    case ERROR:
        return VALUE_ERROR;
    default:
        return VALUE_NIL;
    case NIL:
        {
            Value alternative = VALUE_NIL;
            for (Unt i = 2; i < args -> length; i++) {
                alternative = eval(LIST_GET_UNSAFE(args, i), environment, call_stack);
            }
            return alternative;
        }
    }
}

LISP_BUILTIN(while, "") {
    if (args -> length < 2) {
        return VALUE_ERROR;
    }

    Value condition = LIST_GET_UNSAFE(args, 1);
    while (true) {
        Value result = eval(condition, environment, call_stack);
        result = NILIFY(result);
        switch(result.type) {
        case ERROR:
            return VALUE_ERROR;
        case NIL:
            return VALUE_NIL;
        default:
            for (Unt i = 2; i < args -> length; i++) {
                Value body = LIST_GET_UNSAFE(args, i);
                eval(body, environment, call_stack);
            }
        }
    }
}

LISP_BUILTIN(and, "") {
    Value result = symbols_t;
    for (Unt i = 1; i < args -> length; i++) {
        result = LIST_GET_UNSAFE(args, i);
        switch(NILIFY(result).type) {
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
    for (Unt i = 1; i < args -> length; i++) {
        result = LIST_GET_UNSAFE(args, i);
        switch(NILIFY(result).type) {
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
    if (args -> length < 3) {
        return VALUE_ERROR;
    }

    Value symbol = LIST_GET_UNSAFE(args, 1);
    symbol = NILIFY(symbol);

    Value value = LIST_GET_UNSAFE(args, 2);
    value = NILIFY(value);


    hash_set(environment -> variables, symbol, value);
    return value;
}

LISP_BUILTIN(setq, "") {
    Value symbol;
    Value value;
    Unt i;
    for (i = 0; i < (args -> length - 1) / 2; i++) {
        symbol = LIST_GET_UNSAFE(args, i * 2 + 1);
        symbol = NILIFY(symbol);

        value = LIST_GET_UNSAFE(args, i * 2 + 1 + 1);
        value = eval(value, environment, call_stack);

        if (symbol.type != SYMBOL ||
            symbol.val.symbol_val == symbols_t.val.symbol_val) {
            return VALUE_ERROR;
        }

        hash_set(environment -> variables, symbol, value);
    }

    if (i * 2 < args -> length - 1) {
        symbol = LIST_GET_UNSAFE(args, i * 2 + 1);
        symbol = eval(symbol, environment, call_stack);
        symbol = NILIFY(symbol);

        if (symbol.type != SYMBOL ||
            symbol.val.symbol_val == symbols_t.val.symbol_val) {
            return VALUE_ERROR;
        }

        value = VALUE_NIL;
        hash_set(environment -> variables, symbol, value);
    }
    return value;
}

LISP_BUILTIN(let, "") {
    if (args -> length < 2) {
        return VALUE_ERROR;
    }

    Value pairs_value = LIST_GET_UNSAFE(args, 1);
    List *pairs = pairs_value.val.list_val;

    List *bindings = list_create(round_up_to_power_of_2((pairs -> length - 1) * 2));
    for (Unt i = 0; i < pairs -> length; i++) {
        Value pair = LIST_GET_UNSAFE(pairs, i);
        switch (pair.type) {
        case SYMBOL:
            list_push_back(bindings, pair);
            list_push_back(bindings, VALUE_NIL);
            break;
        case LIST:
            {
                Value symbol;
                Value value;;
                if (pair.val.list_val -> length == 1) {
                    symbol = LIST_GET_UNSAFE(pair.val.list_val, 0);
                    value = VALUE_NIL;
                } else if (pair.val.list_val -> length == 2) {
                    symbol = LIST_GET_UNSAFE(pair.val.list_val, 0);
                    value = LIST_GET_UNSAFE(pair.val.list_val, 1);
                    value = eval(value, environment, call_stack);
                } else {
                    return VALUE_ERROR;
                }
                list_push_back(bindings, symbol);
                list_push_back(bindings, value);
            }
            break;
        default:
            return VALUE_ERROR;
        }
    }

    List *old_bindings = list_create_empty();
    List *not_bound = list_create_empty();
    eval_bind(bindings, environment, old_bindings, not_bound);
    Value result = VALUE_NIL;
    for (Unt i = 2; i < args -> length; i++) {
        Value body = LIST_GET_UNSAFE(args, i);
        result = eval(body, environment, call_stack);
    }
    eval_unbind(environment, old_bindings, not_bound);
    return result;
}

LISP_BUILTIN(let_star, "") {
    if (args -> length < 2) {
        return VALUE_ERROR;
    }

    Value pairs_value = LIST_GET_UNSAFE(args, 1);
    List *pairs = pairs_value.val.list_val;

    List *old_bindings = list_create_empty();
    List *not_bound = list_create_empty();

    for (Unt i = 0; i < pairs -> length; i++) {
        Value pair = LIST_GET_UNSAFE(pairs, i);
        Value symbol;
        Value value = VALUE_NIL;
        switch (pair.type) {
        case SYMBOL:
            symbol = pair;
            break;
        case LIST:
            if (pair.val.list_val -> length == 1) {
                symbol = LIST_GET_UNSAFE(pair.val.list_val, 0);
            } else if (pair.val.list_val -> length == 2) {
                symbol = LIST_GET_UNSAFE(pair.val.list_val, 0);
                value = LIST_GET_UNSAFE(pair.val.list_val, 1);
                value = eval(value, environment, call_stack);
            } else {
                list_destroy(old_bindings);
                list_destroy(not_bound);
                return VALUE_ERROR;
            }
            break;
        default:
            return VALUE_ERROR;
        }
        Value old_value;
        Bool found = hash_get(environment -> variables, symbol, &old_value);
        if (found) {
            list_push_back(old_bindings, symbol);
            list_push_back(old_bindings, old_value);
            /* TODO: increase refcount */
        } else {
            list_push_back(not_bound, symbol);
        }
        hash_set(environment -> variables, symbol, value);
    }

    Value result = VALUE_NIL;
    for (Unt i = 2; i < args -> length; i++) {
        Value body = LIST_GET_UNSAFE(args, i);
        result = eval(body, environment, call_stack);
    }
    eval_unbind(environment, old_bindings, not_bound);
    return result;

}
