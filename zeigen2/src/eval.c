
#include "debug.h"
#include "types.h"
#include "list.h"
#include "eval.h"
#include "symbol.h"
#include "math.h"
#include "hash.h"

Value eval_list(Value expression, Environment *environment, List *call_stack);
Value eval_apply(Value function_symbol, Function *function, List *args, Environment *environment, List *call_stack);
Bool eval_get_bindings(List *args, List *parameters, List *bindings);

Value eval(Value expression, Environment *environment, List *call_stack) {
    switch (expression.type) {
    case ERROR:
    case NIL:
    case INTEGER:
    case FLOAT:
    case STRING:
    case HASH:
    default:
        return expression;
    case SYMBOL: {
        Value result;
        Bool found = hash_get(environment -> variables, expression, &result);
        if (found) {
            return result;
        } else {
            /* TODO: log error */
            return VALUE_ERROR;
        }
    }
    case LIST:
        return eval_list(expression, environment, call_stack);
    }
}

Value eval_list(Value expression, Environment *environment, List *call_stack) {
    /* TODO: assert(list.type == LIST) */
    /* NOTE: Is this necesary? It should already be handled in the parser.
       But it might be needed due to eval */
    List *list = expression.val.list_val;
    if (list -> length <= 0) {
        return VALUE_NIL;
    }

    Value function_symbol = LIST_GET_UNSAFE(list, 0);

    Value function_value;
    Bool found = hash_get(environment -> functions, function_symbol, &function_value);
    if (!found) {
        /* TODO: log error */
        printf("symbol: ");
        print(function_symbol);
        printf(" not found\n");
        return VALUE_ERROR;
    }
    Function *function = function_value.val.function_val;

    List *args = list_create(list -> size);
    if (function -> eval) {
        for (Unt i = 1; i < list -> length; i++) {
            Value evaled_arg = eval(LIST_GET_UNSAFE(list, i), environment, call_stack);
            list_push_back(args, evaled_arg);
        }
    } else {
        for (Unt i = 1; i < list -> length; i++) {
            list_push_back(args, LIST_GET_UNSAFE(list, i));
        }
    }

    Value result;
    if (function -> c_code) {
        result = function -> c_function(args, environment, call_stack);
    } else {
        result = eval_apply(function_symbol, function, args, environment, call_stack);
    }
    list_destroy(args);
    return result;
}

Value eval_apply(Value function_symbol, Function *function, List *args, Environment *environment, List *call_stack) {
    List *bindings = list_create(args -> size);
    Bool bindings_wellformed = eval_get_bindings(args, function -> parameters, bindings);
    if (!bindings_wellformed) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    List *old_bindings = list_create_empty();
    List *not_bound = list_create_empty();
    eval_bind(bindings, environment, old_bindings, not_bound);

    list_push_back(call_stack, function_symbol);
    Value result = eval(function -> body, environment, call_stack);
    list_pop_back(call_stack);

    eval_unbind(environment, old_bindings, not_bound);
    list_destroy(old_bindings);
    list_destroy(not_bound);
    return result;
}

Bool eval_get_bindings(List *arguments, List *parameters, List *bindings) {
    /* Most of the error logging should be in defun, not here! */
    Bool optional = false;
    Bool rest = false;
    Unt a = 0;
    Unt p = 0;
    while (a < arguments -> length && p < parameters -> length) {
        Value parameter = LIST_GET_UNSAFE(parameters, p);
        if (parameter.val.symbol_val == symbols_ampersand_optional.val.symbol_val) {
            optional = true;
            p++;
            break;
        }
        if (parameter.val.symbol_val == symbols_ampersand_rest.val.symbol_val) {
            rest = true;
            p++;
            break;
        }
        list_push_back(bindings, parameter);
        Value argument = LIST_GET_UNSAFE(arguments, a);
        list_push_back(bindings, argument);
        a++, p++;
    }

    if (optional) {
        while (p < parameters -> length) {
            Value parameter = LIST_GET_UNSAFE(parameters, p);
            if (parameter.val.symbol_val == symbols_ampersand_rest.val.symbol_val) {
                rest = true;
                p++;
                break;
            }
            list_push_back(bindings, parameter);
            Value argument;
            if (a < arguments -> length) {
                argument = LIST_GET_UNSAFE(arguments, a);
                a++;
            } else {
                argument = VALUE_NIL;
            }
            list_push_back(bindings, argument);
            p++;
        }
    }

    if (rest) {
        if (parameters -> length - p >= 1) {
            /* TODO: log error, too many rest params */
            return false;
        } else if (parameters -> length - p == 0) {
            /* TODO: log error, missing rest params */
            return false;
        } else {
            Value parameter = LIST_GET_UNSAFE(parameters, p);
            Unt rest_length = arguments -> length - a;
            List *rest = list_create(round_up_to_power_of_2(rest_length));
            while (a < arguments -> length) {
                list_push_back(rest, LIST_GET_UNSAFE(arguments, a));
                a++;
            }
            list_push_back(bindings, parameter);
            list_push_back(bindings, VALUE_LIST(rest));
        }
    }


    if (a >= arguments -> length || p >= parameters -> length) {
        /* TODO: log error */
        return false;
    }
    return true;
}

void eval_bind(List *bindings, Environment *environment, List *old_bindings, List *not_bound) {
    for (Unt i = 0; i < bindings -> length; i += 2) {
        Value parameter = LIST_GET_UNSAFE(bindings, i);
        Value argument = LIST_GET_UNSAFE(bindings, i+1);
        Value old_value;
        Bool found = hash_get(environment -> variables, parameter, &old_value);
        if (found) {
            list_push_back(old_bindings, parameter);
            list_push_back(old_bindings, old_value);
            /* TODO: increase refcount */
        } else {
            list_push_back(not_bound, parameter);
        }
        /* Note: performance could be improved with a combined get old/insert new, "hash_replace" method */
        hash_set(environment -> variables, parameter, argument);
    }
}

void eval_unbind(Environment *environment, List *old_bindings, List *not_bound) {
    for (Unt i = 0; i < old_bindings -> length; i += 2) {
        Value symbol = LIST_GET_UNSAFE(old_bindings, i);
        Value old_value = LIST_GET_UNSAFE(old_bindings, i+1);
        hash_set(environment -> variables, symbol, old_value);
    }
    for (Unt i = 0; i < not_bound -> length; i++) {
        Value symbol = LIST_GET_UNSAFE(not_bound, i);
        hash_delete(environment -> variables, symbol);
    }
}
