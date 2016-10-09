#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../eval.h"
#include "../string.h"
#include "../math.h"
#include "../memory.h"
#include "../debug.h"

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

    Function *function = memory_malloc(sizeof(Function));
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

    Value parameters = CAR(args);
    if (!IS_LIST(parameters)) {
        return VALUE_ERROR;
    }
    return CONS(symbols_lambda, args);
}
