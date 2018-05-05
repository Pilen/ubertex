#include "../headers.h"

LISP_BUILTIN(defun, "") {
    ENSURE_NOT_EMPTY(args);
    Value function_name = NEXT(args);
    if (function_name.type != SYMBOL) {
        return VALUE_ERROR;
    }

    ENSURE_NOT_EMPTY(args);
    Value parameters = NEXT(args);
    /* TODO: validate parameters */
    if (!IS_LIST(parameters)) {
        return VALUE_ERROR;
    }

    Value body = CONS(symbols_progn, args);

    String *docstring = NULL;
    if (args.type == CONS) {
        if (CAR(args).type == STRING) {
            docstring = CAR(args).val.string_val;
        } else {
            docstring = string_create_from_str("Undocumented function");
        }
    }

    Function *function = NEW(Function);
    function -> eval = true;
    function -> c_code = false;
    function -> c_function = NULL;
    function -> parameters = parameters;
    function -> body = body;
    function -> docstring = docstring;
    Value function_value = VALUE_FUNCTION(function);

    hash_set(environment -> functions, function_name, function_value);
    return function_name;
}

LISP_BUILTIN(lambda, "") {
    ENSURE_NOT_EMPTY(args);

    Value parameters = NEXT(args);
    if (!IS_LIST(parameters)) {
        return VALUE_ERROR;
    }

    Value body = CONS(symbols_progn, args);

    String *docstring = NULL;
    if (args.type == CONS) {
        if (CAR(args).type == STRING) {
            docstring = CAR(args).val.string_val;
        } else {
            docstring = string_create_from_str("Undocumented function");
        }
    }

    Lambda *lambda = NEW(Lambda);
    lambda -> parameters = parameters;
    lambda -> body = body;
    lambda -> docstring = docstring;
    lambda -> lexical = environment -> lexical_variables;

    return VALUE_LAMBDA(lambda);
}

LISP_BUILTIN(lexical_let, "") {
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
                // pair is now the second element
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
    environment_bind_lexical(bindings, environment);
    environment_bind_variables(bindings, environment);
    Value result = VALUE_NIL;
    while (args.type == CONS) {
        Value body = NEXT(args);
        result = eval(body, environment);
    }
    environment_unbind_variables(environment);
    environment_unbind_lexical(environment);

    return result;
}

LISP_BUILTIN(funcall, "") {
    ENSURE_NOT_EMPTY(args);
    Value function_name = NEXT(args);
    return eval(CONS(function_name, args), environment);
}
