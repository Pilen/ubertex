
#include "headers.h"

/* Note: decide if this function should just be located in initialize.c */
Environment *environment_create(void) {
    Environment *environment = NEW(Environment);
    environment -> window = NULL;
    environment -> renderer = NULL;
    environment -> base_texture = NULL;
    environment -> cairo_surface = NULL;
    environment -> cairo = NULL;

    environment -> frame = 0;

    environment -> fast_run = false;

    environment -> width = 0;
    environment -> height = 0;

    environment -> component_definitions = hash_create();
    environment -> current_component = NULL;
    environment -> current_layer = OPTION_DEFAULT_LAYER;
    environment -> layers_foreground = component_layer_create(OPTION_DEFAULT_LAYER);
    environment -> layers_background = component_layer_create(OPTION_DEFAULT_LAYER - 1);

    environment -> messages = VALUE_NIL;
    environment -> last_message = VALUE_NIL;

    environment -> update = VALUE_NIL;
    environment -> background = VALUE_NIL;
    environment -> foreground = VALUE_NIL;

    environment -> clear_red = 0.0;
    environment -> clear_green = 0.0;
    environment -> clear_blue = 0.0;
    environment -> clear_alpha = 1.0;

    environment -> dynamic_variables = VALUE_NIL;
    environment -> lexical_variables = VALUE_NIL;
    environment -> global_variables = hash_create();
    environment -> functions = hash_create();

    environment -> call_stack = VALUE_NIL; /* list */
    return environment;
}

void environment_bind_variables(Value bindings, Environment *environment) {
    environment -> dynamic_variables = CONS(bindings, environment -> dynamic_variables);
}

void environment_unbind_variables(Environment *environment) {
    environment -> dynamic_variables = CDR(environment -> dynamic_variables);
}

void environment_bind_lexical(Value bindings, Environment *environment) {
    environment -> lexical_variables = CONS(bindings, environment -> lexical_variables);
}

void environment_unbind_lexical(Environment *environment) {
    environment -> lexical_variables = CDR(environment -> lexical_variables);
}
Unt environment_bind_multiple_variables(Value bindings, Environment *environment) {
    Value ordered = list_reverse(list_copy(bindings));
    Unt count = 0;
    while (ordered.type == CONS) {
        count++;
        Value actual_bindings = NEXT(ordered);
        environment -> dynamic_variables = CONS(actual_bindings, environment -> dynamic_variables);
    }
    w_assert(ordered.type == NIL);
    return count;
}
void environment_unbind_multiple_variables(Unt count, Environment *environment) {
    while (count) {
        count--;
        environment -> dynamic_variables = CDR(environment -> dynamic_variables);
    }
}

Bool environment_lookup_variable(Value key, Value *result, Environment *environment) {
    /* Cant simply return value or nil/error, as we cant distinguish values not found to ones bound */
    Value groups = environment -> dynamic_variables;
    while (groups.type == CONS) {
        Value bindings = NEXT(groups);
        while (bindings.type == CONS) {
            Value binding = NEXT(bindings);
            if (equal(key, CAR(binding))) {
                *result = CDR(binding);
                return true;
            }
        }
    }
    Bool found = hash_get(environment -> global_variables, key, result);
    return found;
}

void environment_set_variable(Value key, Value value, Environment *environment) {
    Value groups = environment -> dynamic_variables;
    while (groups.type == CONS) {
        Value bindings = NEXT(groups);
        while (bindings.type == CONS) {
            Value binding = NEXT(bindings);
            if (equal(key, CAR(binding))) {
                CDR(binding) = value;
                return;
            }
        }
    }
    hash_set(environment -> global_variables, key, value);
}
