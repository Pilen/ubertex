#include "component.h"
#include "memory.h"
#include "environment.h"
#include "lisp.h"
#include "hash.h"
#include "eval.h"

void component_execute(Value symbol, Value args, Environment *environment);

void component_define(Value name, Value constructor, Value parameters, String *docstring, Value body, Environment *environment) {
    Function *function = memory_malloc(sizeof(Function));
    function -> eval = true;
    function -> c_code = false;
    function -> c_function = NULL;
    function -> parameters = parameters;
    function -> body = body;
    function -> docstring = docstring;
    Value function_value = VALUE_FUNCTION(function);
    hash_set(environment -> component_definitions, name, function_value);
}

Component *component_create(Value name, Value args, Environment *environment) {
    Value definition;
    Bool found = hash_get(environment -> component_definitions, name, &definition);
    if (!found) {
        return NULL;
    }

    Component *component = memory_malloc(sizeof(Component));
    component -> name = name;
    component -> local_variables = VALUE_NIL;
    component -> update = VALUE_NIL;
    component -> update_arguments = VALUE_NIL;
    component -> render = VALUE_NIL;
    component -> render_arguments = VALUE_NIL;
    component -> message_queue = VALUE_NIL;
    Value component_value = VALUE_COMPONENT(component);
    eval_apply(component_value, definition.val.function_val, args, environment);
    return component;
}

void component_update_all(Component *component, Environment *environment) {
    environment -> current_component = component;
    component_execute(component -> update, component -> update_arguments, environment);
    component_execute(component -> render, component -> render_arguments, environment);
}

void component_execute(Value symbol, Value args, Environment *environment) {
    /* Lookup must be done every frame as the body can be redefined. */
    /* TODO: or a lambda! */
    if (symbol.type == SYMBOL) {
        Value function_value;
        Bool found = hash_get(environment -> functions, symbol, &function_value);
        if (found) {
            Function *update_function = function_value.val.function_val;
            /* Is evaled when the update function is set, not now */
            eval_apply(symbol, update_function, args, environment);
        } else {
            /* TODO: log error better*/
            log_error("Error when updating component, no such function");
        }
    } else if (symbol.type == NIL) {
        /* Do nothing */
    } else {
        log_error("Error when updating component, not a function");
    }
}


Layer *component_layer_create(Int index) {
    Layer *layer = memory_malloc(sizeof(Layer));
    layer -> index = index;
    layer -> entries = VALUE_NIL;
    layer -> lower = NULL;
    layer -> higher = NULL;
    return layer;
}

void component_layer_insert_component(Int index, Value component, Environment *environment) {
    Layer *layer = environment -> layers;
    while (true) {
        if (index == layer -> index) {
            break;
        } else if (index < layer -> index) {
            if (layer -> lower) {
                layer = layer -> lower;
            } else {
                layer = component_layer_create(index);
                layer -> lower = layer;
                break;
            }
        } else { // index > layer -> index
            if (layer -> higher) {
                layer = layer -> higher;
            } else {
                layer = component_layer_create(index);
                layer -> higher = layer;
                break;
            }
        }
    }

    if (layer -> entries.type == NIL) {
        layer -> entries = CONS1(component);
        layer -> last_entry = layer -> entries;
    } else {
        CDR(layer -> last_entry) = CONS1(component);
    }
}
