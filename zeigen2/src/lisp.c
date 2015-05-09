#include "debug.h"

#include "lisp.h"
#include "symbol.h"
#include "memory.h"
#include "string.h"


void lisp_initialize(Environment *environment) {
    LISP_REGISTER_BUILTIN(symbols_progn, progn, false, environment);
    LISP_REGISTER_BUILTIN(symbols_quote, quote, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(eval, eval, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(list, list, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(if, if, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(when, when, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(unless, unless, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(while, while, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(and, and, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(or, or, false, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(set, set, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(setq, setq, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(let, let, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(let*, let_star, false, environment);


    LISP_REGISTER_BUILTIN_FROM_RAW(eq, eq, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(equal, equal, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(not, not, true, environment);



    LISP_REGISTER_BUILTIN_FROM_RAW(defun, defun, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(lambda, lambda, false, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(+, plus, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(-, minus, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(-, update, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(-, event, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(-, draw, true, environment);
}

void lisp_register_builtin(Value symbol, c_lisp_function c_function, Bool eval, String *docstring, Environment *environment) {
    Function *function = z_malloc(sizeof(Function));
    function -> eval = eval;
    function -> c_code = true;
    function -> c_function = c_function;
    function -> parameters = NULL;
    function -> body = (Value) {ERROR, {0}}; /* Done this way to avoid logging */
    function -> docstring = docstring;
    Value function_value = VALUE_FUNCTION(function);

    hash_set(environment -> functions, symbol, function_value);
}
