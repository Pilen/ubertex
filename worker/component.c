
#include "headers.h"

Component *component_create(Value name, Value args, Environment *environment) {
    Value definition;
    Bool found = hash_get(environment -> component_definitions, name, &definition);
    if (!found) {
        return NULL;
    }
    Function *constructor = definition.val.function_val;

    Component *component = NEW(Component);
    component -> name = name;
    component -> layer = NULL;
    component -> local_variables = VALUE_NIL;
    component -> update = VALUE_NIL;
    component -> background = VALUE_NIL;
    component -> foreground = VALUE_NIL;
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


void component_update_layers(Layer *layer, Environment *environment) {
    while (layer) {
        environment -> current_layer = layer -> index;
        Value components = layer -> entries;
        while (components.type == CONS) {
            Value component_value = NEXT(components);
            Component *component = component_value.val.component_val;
            environment -> current_component = component;
            environment_bind_variables(component -> local_variables, environment);
            eval(component -> background, environment);
            eval(component -> update, environment);
            eval(component -> foreground, environment);
            environment_unbind_variables(environment);
        }
        layer = layer -> next;
    }
}
void component_update_all(Environment *environment) {
    component_update_layers(environment -> layers_background, environment);
    component_update_layers(environment -> layers_foreground, environment);
    environment -> current_layer = OPTION_DEFAULT_LAYER;
    environment -> current_component = NULL;
}


Layer *component_layer_create(Int index) {
    Layer *layer = NEW(Layer);
    layer -> index = index;
    layer -> entries = VALUE_NIL;
    layer -> next = NULL;
    layer -> last_entry = VALUE_NIL;
    return layer;
}

Layer *component_layer_insert_component_helper(Int index, Component *component, Layer *layers) {
    /* Assume environment -> layers_* are sorted sequences [-inf: -1] and [0 : inf] */
    Layer *layer = layers;
    if (index < layer -> index) {
        Layer *next = layer;
        layer = component_layer_create(index);
        layer -> next = next;
    } else {
        while (true) {
            if (index == layer -> index) {
                break;
            }
            Layer *next = layer -> next;
            if (!next || index < next -> index) {
                layer -> next = component_layer_create(index);
                layer = layer -> next;
                layer -> next = next;
                break;
            }
            layer = next;
        }
    }

    /* insert into layer */
    if (layer -> entries.type == NIL) {
        layer -> entries = CONS1(VALUE_COMPONENT(component));
        layer -> last_entry = layer -> entries;
    } else {
        Value new = CONS1(VALUE_COMPONENT(component));
        CDR(layer -> last_entry) = new;
        layer -> last_entry = new;
    }
    component -> layer = layer;
    return layers;
}

void component_layer_insert_component(Int index, Component *component, Environment *environment) {
    if (index >= OPTION_DEFAULT_LAYER) {
        environment -> layers_foreground = component_layer_insert_component_helper(index, component, environment -> layers_foreground);
    } else {
        environment -> layers_background = component_layer_insert_component_helper(index, component, environment -> layers_background);
    }
}


void component_destroy_all_helper(Layer *layers) {
    Layer *layer = layers;
    while (layer) {
        Value components = layer -> entries;
        while (components.type == CONS) {
            Value component = NEXT(components);
            component.val.component_val -> layer = NULL;
        }
        layer = layer -> next;
    }
}
void component_destroy_all(Environment *environment) {
    component_destroy_all_helper(environment -> layers_foreground);
    component_destroy_all_helper(environment -> layers_background);
    environment -> current_layer = OPTION_DEFAULT_LAYER;
    environment -> layers_foreground = component_layer_create(OPTION_DEFAULT_LAYER);
    environment -> layers_background = component_layer_create(OPTION_DEFAULT_LAYER - 1);
}

/* Remove component from layers.
   Does not mark it destroyed, so can be used for moving between layers */
void component_remove(Component *component, Environment *environment) {
    (void) environment; /* Environment not actually used */
    w_assert(component -> layer);
    Layer *layer = component -> layer;
    component -> layer = NULL;
    Value entries = layer -> entries;
    w_assert(entries.type == CONS); /* The component must exist in this layer */
    if (CAR(entries).val.component_val == component) {
        if (CDR(entries).type == NIL) {
            layer -> last_entry = VALUE_NIL;
        }
        layer -> entries = CDR(entries);
        return;
    }
    while (true) {
        w_assert(CDR(entries).type == CONS); /* The component must exist in this layer */
        if (CAR(CDR(entries)).val.component_val == component) {
            if (CDR(CDR(entries)).type == NIL) {
                layer -> last_entry = entries;
            }
            CDR(entries) = CDR(CDR(entries));
            return;
        }
        (void) NEXT(entries);
    }
    w_assert(false);
}
