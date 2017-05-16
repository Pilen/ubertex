#include "component.h"
#include "memory.h"
#include "environment.h"
#include "lisp.h"
#include "hash.h"
#include "eval.h"
#include "assert.h"
#include "debug.h"
#include "symbol.h"

void component_execute(Value symbol, Value args, Environment *environment);

Component *component_create(Value name, Value args, Environment *environment) {
    Value definition;
    Bool found = hash_get(environment -> component_definitions, name, &definition);
    if (!found) {
        return NULL;
    }
    Function *constructor = definition.val.function_val;

    Component *component = memory_malloc(sizeof(Component));
    component -> name = name;
    component -> layer = NULL;
    component -> local_variables = VALUE_NIL;
    component -> update = VALUE_NIL;
    component -> render = VALUE_NIL;
    component -> message_handlers = hash_create();

    component_layer_insert_component(environment -> current_layer, component, environment);

    Component *previous_component = environment -> current_component;
    environment -> current_component = component;
    /* eval_apply cant be used as we have to bind the parameters locally,
       not dynamically, and keep them. */
    Value bindings = eval_get_bindings(args, constructor -> parameters);
    /* Done this way as to enable pushes to front (via an explicit mutable head of unreachable symbol) */
    component -> local_variables = CONS(CONS(symbols_unreachable, VALUE_ERROR),
                                        bindings);
    environment_bind_variables(component -> local_variables, environment);
    eval(constructor -> body, environment); /* Discard return value */
    environment_unbind_variables(environment);
    environment -> current_component = previous_component;

    return component;
}

void component_update_all(Environment *environment) {
    Layer *layer = environment -> layers;
    while (layer) {
        environment -> current_layer = layer -> index;
        Value components = layer -> entries;
        while (components.type == CONS) {
            Value component_value = NEXT(components);
            Component *component = component_value.val.component_val;
            environment -> current_component = component;
            environment_bind_variables(component -> local_variables, environment);
            eval(component -> update, environment);
            eval(component -> render, environment);
            /* component_execute(component -> update, component -> update_arguments, environment); */
            /* component_execute(component -> render, component -> render_arguments, environment); */
            environment_unbind_variables(environment);
        }
        layer = layer -> next;
    }
    environment -> current_layer = OPTION_DEFAULT_LAYER;
    environment -> current_component = NULL;
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
    layer -> next = NULL;
    return layer;
}

void component_layer_insert_component(Int index, Component *component, Environment *environment) {
    /* Assume environment -> layers is a sorted sequence */
    Layer *layer = environment -> layers;
    if (index < layer -> index) {
        Layer *next = layer;
        layer = component_layer_create(index);
        environment -> layers = layer;
        layer -> next = next;
        layer -> entries = CONS1(VALUE_COMPONENT(component));
        layer -> last_entry = layer -> entries;
        component -> layer = layer;
        return;
    }

    while (true) {
        if (index == layer -> index) {
            break;
        }
        Layer *next = layer -> next;
        if (!next) {
            layer -> next = component_layer_create(index);
            layer = next;
            break;
        }
        if (index > next -> index) {
            layer -> next = component_layer_create(index);
            layer = layer -> next;
            layer -> next = next;
            break;
        }
    }

    /* insert into layer */
    if (layer -> entries.type == NIL) {
        layer -> entries = CONS1(VALUE_COMPONENT(component));
        layer -> last_entry = layer -> entries;
    } else {
        /* TODO: need a test for this part */
        Value new = CONS1(VALUE_COMPONENT(component));
        CDR(layer -> last_entry) = new;
        layer -> last_entry = new;


    }
    component -> layer = layer;
}

void component_destroy_all(Environment *environment) {
    Layer *layer = environment -> layers;
    while (layer) {
        Value components = layer -> entries;
        while (components.type == CONS) {
            Value component = NEXT(components);
            component.val.component_val -> layer = NULL;
        }
        layer = layer -> next;
    }
    environment -> current_layer = OPTION_DEFAULT_LAYER;
    environment -> layers = component_layer_create(OPTION_DEFAULT_LAYER);
}

/* Remove component from layers.
   Does not mark it destroyed, so can be used for moving between layers */
void component_remove(Component *component, Environment *environment) {
    w_assert(component -> layer);
    Layer *layer = component -> layer;
    component -> layer = NULL;
    Value entries = layer -> entries;
    w_assert(entries.type == CONS); /* The component must exist in this layer */
    if (CAR(entries).val.component_val == component) {
        layer -> entries = CDR(entries);
        return;
    }
    while (true) {
        w_assert(CDR(entries).type == CONS); /* The component must exist in this layer */
        if (CAR(CDR(entries)).val.component_val == component) {
            CDR(entries) = CDR(CDR(entries));
            return;
        }
        (void) NEXT(entries);
    }
    w_assert(false);
}
