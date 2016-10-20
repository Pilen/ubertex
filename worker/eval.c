
#include "debug.h"
#include "types.h"
#include "list.h"
#include "eval.h"
#include "symbol.h"
#include "math.h"
#include "hash.h"
#include "log.h"
#include "assert.h"
#include "loop.h"

Value eval_list(Value expression, Environment *environment);
Value eval_apply(Value function_symbol, Function *function, Value args, Environment *environment);
Value eval_get_bindings(Value arguments, Value parameters);
Value eval_lambda(Value lambda, Value args, Environment *environment);

Value eval(Value expression, Environment *environment) {
    switch (expression.type) {
        /* Self evaluating: */
    case ERROR:
    case NIL:
    case INTEGER:
    case FLOAT:
    case STRING:
    case VECTOR:
    case HASH:
    default:
        return expression;
    case SYMBOL: {
        Value result;
        Bool found = environment_lookup_variable(expression, &result, environment);
        if (found) {
            return result;
        } else {
            /* TODO: log error */
            /* TODO: "Did you mean?" */
            debug_value(expression);
            log_error("Variable XXX not found");
            return VALUE_ERROR;
        }
    }
    case CONS:
        return eval_list(expression, environment);
    }
}

Value eval_list(Value expression, Environment *environment) {
    w_assert(expression.type == CONS);

    Value function_symbol = NEXT(expression);
    if (function_symbol.type == CONS) {
        return eval_lambda(function_symbol, expression, environment);
    }

    w_assert(function_symbol.type == SYMBOL);
    Value function_value;
    Bool found = hash_get(environment -> functions, function_symbol, &function_value);
    if (!found) {
        /* TODO: log error */
        /* TODO: "Did you mean?" */
        debug_value(function_symbol);
        log_error("Function XXX not found");
        return VALUE_ERROR;
    }
    w_assert(function_value.type == FUNCTION);
    Function *function = function_value.val.function_val;

    Value args;
    if (function -> eval) {
        args = VALUE_NIL;
        while (expression.type == CONS) {
            Value arg = NEXT(expression);
            args = CONS(eval(arg, environment), args);
        }
        args = list_reverse(args);
        w_assert(expression.type == NIL);

        /* TODO: benchmark, which approach is better, the above or below? */

        /* if (expression.type == CONS) { */
        /*     args = CONS1(VALUE_NIL); */
        /* } else { */
        /*     args = expression; */
        /* } */
        /* Cons *top = args.val.cons_val; */
        /* while (true) { */
        /*     Value arg = NEXT(expression); */
        /*     top -> car = eval(arg, environment); */
        /*     if (expression.type == CONS) { */
        /*         top -> cdr = CONS1(VALUE_NIL); */
        /*         top = top -> cdr.val.cons_val; */
        /*     } else { */
        /*         top -> cdr = expression; */
        /*         break; */
        /*     } */
        /* } */
    } else {
        /* To ensure we avoid mutation in altering code the list is copied
           If we guaranteed that no function with eval = false modifies the list we could give it directly
           This would be an obvious performance optimization. But needs tests.
           We can only guarantee this for c_code, not for userdefined macros.
           TODO: Do this */
        args = list_copy(expression);
    }

    Value result = eval_apply(function_symbol, function, args, environment);
    list_destroy(args);
    return result;
}

Value eval_apply(Value function_symbol, Function *function, Value args, Environment *environment) {
    /* All functions called from apply can be safe to assume ``args'' is a proper list
       Ensure this! */
    if (loop_abort) {
        return VALUE_ERROR;
    }
    if (function -> c_code) {
        return function -> c_function(args, environment);
    }
    Value bindings = eval_get_bindings(args, function -> parameters);
    if (bindings.type == ERROR) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    environment_bind_variables(bindings, environment);
    environment -> call_stack = CONS(function_symbol, environment -> call_stack);
    Value result = eval(function -> body, environment);
    environment -> call_stack = CDR(environment -> call_stack);
    environment_unbind_variables(environment);

    return result;
}

Value eval_lambda(Value lambda, Value args, Environment *environment) {
    w_assert(false);
    return VALUE_ERROR;
    Value head = CAR(lambda);
    Bool clojure;
    if (head.type == SYMBOL && head.val.symbol_val == symbols_lambda.val.symbol_val) {
        clojure = false;
    } else if (head.type == SYMBOL && head.val.symbol_val == symbols_clojure.val.symbol_val) {
        clojure = true;
    } else {
        return VALUE_ERROR;
    }
    /* Eval args */
    Value rest = args;
    args = VALUE_NIL;
    while (rest.type == CONS) {
        Value arg = NEXT(rest);
        args = CONS(eval(arg, environment), args);
    }
    w_assert(rest.type == NIL);
    args = list_reverse(args);
    args = CONS(symbols_lambda, args);
    Value result;
    if (clojure) {
        /* result = eval_clojure(function_symbol, args, environment); */
    } else {
        /* result = eval_lambda(function_symbol, args, environment); */
    }
    list_destroy(args);
    return result;

}

Value eval_get_bindings(Value arguments, Value parameters) {
    /* Returns bindings in opposite direction (first is tightest bound) */
    w_assert(IS_LIST(arguments));
    w_assert(IS_LIST(parameters));
    /* Most of the error logging should be in defun, not here! */
    Bool optional = false;
    Bool rest = false;
    Value bindings = VALUE_NIL;
    while (parameters.type == CONS) {
        Value parameter = NEXT(parameters);
        if (parameter.val.symbol_val == symbols_ampersand_optional.val.symbol_val) {
            optional = true;
            break;
        }
        if (parameter.val.symbol_val == symbols_ampersand_rest.val.symbol_val) {
            rest = true;
            break;
        }
        ENSURE_NOT_EMPTY(arguments);
        Value argument = NEXT(arguments);
        bindings = CONS(CONS(parameter, argument), bindings);
    }

    if (optional) {
        while (parameters.type == CONS) {
            Value parameter = NEXT(parameters);
            if (parameter.val.symbol_val == symbols_ampersand_rest.val.symbol_val) {
                rest = true;
                break;
            }
            Value argument;
            if (arguments.type == CONS) {
                argument = NEXT(arguments);
            } else {
                argument = VALUE_NIL;
            }
            bindings = CONS(CONS(parameter, argument), bindings);
        }
    }

    if (rest) {
        if (parameters.type == CONS) {
            Value parameter = NEXT(parameters);
            bindings = CONS(CONS(parameter, arguments), bindings);
            arguments = VALUE_NIL;
            /* It would make sense to only allow one rest parameter
               But as Emacs allows more (or none) so do we.
               But defun could definitely give a warning */
            while (parameters.type == CONS) {
                parameter = NEXT(parameters);
                bindings = CONS(CONS(parameter, VALUE_NIL), bindings);
                arguments = VALUE_NIL;
            }
        } else {
            arguments = VALUE_NIL;
            parameters = VALUE_NIL;
        }
    }
    ENSURE_EMPTY(arguments);
    ENSURE_EMPTY(parameters);
    /* w_assert(parameters.type == NIL); */
    /* w_assert(arguments.type == NIL); */
    return bindings;
}
