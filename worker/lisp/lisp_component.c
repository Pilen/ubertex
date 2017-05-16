#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../resource.h"
#include "../graphics.h"
#include "../string.h"
#include "../component.h"
#include "../assert.h"

LISP_BUILTIN(define_component, "") {
    ENSURE_NOT_EMPTY(args);
    Value component_name = NEXT(args);
    if (component_name.type != SYMBOL) {
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
            docstring = string_create_from_str("Undocumented component");
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
    hash_set(environment -> component_definitions, component_name, function_value);

    return component_name;
}

LISP_BUILTIN(create_component, "") {
    ENSURE_NOT_EMPTY(args);
    Value name = NEXT(args);
    Value evaled = VALUE_NIL;
    while (args.type == CONS) {
        Value arg = NEXT(args);
        evaled = CONS(eval(arg, environment), evaled);
    }
    evaled = list_reverse(evaled);
    Component *component = component_create(name, evaled, environment);
    if (!component) {
        /* TODO: log error */
        log_error("No such component");
        return VALUE_ERROR;
    }
    return VALUE_COMPONENT(component);
}

LISP_BUILTIN(destroy_component, "") {
    Component *component;
    if (args.type == NIL) {
        component = environment -> current_component;
    } else {
        Value component_value = NEXT(args);
        if (component_value.type == COMPONENT) {
            component = component_value.val.component_val;
        } else {
            return VALUE_ERROR;
        }
    }
    if (!component -> layer) {
        return VALUE_ERROR;
    }
    ENSURE_EMPTY(args);
    component_remove(component, environment);
    return VALUE_NIL;
}

LISP_BUILTIN(set_layer, "") {
    ENSURE_NOT_EMPTY(args);
    Value index_value = NEXT(args);
    if (index_value.type != INTEGER) {
        return VALUE_ERROR;
    }
    Int index = index_value.val.integer_val;
    Component *component;
    if (args.type == NIL) {
        component = environment -> current_component;
        w_assert(component -> layer);
    } else {
        Value component_value = NEXT(args);
        if (component_value.type != COMPONENT) {
            return VALUE_ERROR;
        }
        component = component_value.val.component_val;
        if (!component -> layer) {
            return VALUE_ERROR;
        }
    }
    ENSURE_EMPTY(args);
    component_remove(component, environment);
    component_layer_insert_component(index, component, environment);
    return VALUE_NIL;
}

LISP_BUILTIN(current_layer, "") {
    ENSURE_EMPTY(args);
    return VALUE_INTEGER(environment -> current_layer);
}

LISP_BUILTIN(deflocal, "") {
    ENSURE_NOT_EMPTY(args);
    Value symbol = NEXT(args);
    if (symbol.type != SYMBOL) {
        return VALUE_ERROR;
    }
    Value value = VALUE_NIL;

    if (args.type == CONS) {
        value = NEXT(args);
        value = eval(value, environment);
    }

    if (!environment -> current_component) {
        return VALUE_ERROR;
    }
    /* Done this way as to enable pushes to front */
    Value new = CONS(CONS(symbol, value),
                     CDR(environment -> current_component -> local_variables));
    CDR(environment -> current_component -> local_variables) = new;

    return VALUE_NIL;
}


LISP_BUILTIN(update, "") {
    Value body = VALUE_NIL;
    if (args.type == CONS) {
        if (CDR(args).type == NIL) {
            body = CAR(args);
        } else {
            body = CONS(symbols_progn, args);
        }
    }
    if (environment -> current_component) {
        environment -> current_component -> update = body;
    } else {
        environment -> update = body;
    }
    return VALUE_NIL;
}

LISP_BUILTIN(render, "") {
    Value body = VALUE_NIL;
    if (args.type == CONS) {
        if (CDR(args).type == NIL) {
            body = CAR(args);
        } else {
            body = CONS(symbols_progn, args);
        }
    }
    if (environment -> current_component) {
        environment -> current_component -> render = body;
    } else {
        environment -> render = body;
    }
    return VALUE_NIL;
}
