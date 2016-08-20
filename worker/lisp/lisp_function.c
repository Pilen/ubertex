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
    if (args -> length < 3) {
        return VALUE_ERROR;
    }

    Value function_name = LIST_GET_UNSAFE(args, 1);
    Value parameters = LIST_GET_UNSAFE(args, 2);
    String *docstring = NULL;

    if (function_name.type != SYMBOL) {
        return VALUE_ERROR;
    }

    List *param_list;
    if (parameters.type == LIST) {
        param_list = parameters.val.list_val;
    } else if (parameters.type == NIL) {
        param_list = list_create_empty();
    } else {
        return VALUE_ERROR;
    }
    /* TODO: validate parameters */

    if (args -> length >= 4) {
        Value docstring_value = LIST_GET_UNSAFE(args, 3);
        if (docstring_value.type == STRING) {
            docstring = docstring_value.val.string_val;
        }
    }
    if (!docstring) {
        docstring = string_create_from_str("Undocumented function");
    }

    List *body = list_create(round_up_to_power_of_2(args -> length - 3 + 1));
    list_push_back(body, symbols_progn);
    for (Unt i = 3; i < args -> length; i++) {
        Value statement = LIST_GET_UNSAFE(args, i);
        list_push_back(body, statement);
    }

    Function *function = memory_malloc(sizeof(Function));
    function -> eval = true;
    function -> c_code = false;
    function -> c_function = NULL;
    function -> parameters = param_list;
    function -> body = VALUE_LIST(body);
    function -> docstring = docstring;
    Value function_value = VALUE_FUNCTION(function);

    hash_set(environment -> functions, function_name, function_value);
    return function_name;
}

LISP_BUILTIN(lambda, "") {
    if (args -> length < 2) {
        return VALUE_ERROR;
    }
    Value params = LIST_GET_UNSAFE(args, 1);
    if (params.type != NIL && params.type != LIST) {
        return VALUE_ERROR;
    }

    return VALUE_LIST(args);
}
