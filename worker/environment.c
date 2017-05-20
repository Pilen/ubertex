#include "types.h"
#include "environment.h"
#include "basic.h"
#include "hash.h"
#include "memory.h"
#include "list.h"
#include "component.h"

/* Note: decide if this function should just be located in initialize.c */
Environment *environment_create(void) {
    Environment *environment = memory_malloc(sizeof(Environment));
    environment -> window = NULL;
    environment -> renderer = NULL;
    environment -> base_texture = NULL;
    environment -> cairo_surface = NULL;
    environment -> cairo = NULL;

    environment -> frame = 0;

    environment -> skip_ticks = 1000 / OPTION_DEFAULT_FPS;
    environment -> fast_run = false;

    environment -> width = 0;
    environment -> height = 0;

    environment -> component_definitions = hash_create();
    environment -> current_component = NULL;
    /* environment -> current_layer = OPTION_DEFAULT_LAYER; */
    /* environment -> layers = component_layer_create(environment -> current_layer); */
    component_destroy_all(environment); /* initializes all */

    environment -> update = VALUE_NIL;
    environment -> background = VALUE_NIL;
    environment -> foreground = VALUE_NIL;

    environment -> clear_red = 0.0;
    environment -> clear_green = 0.0;
    environment -> clear_blue = 0.0;
    environment -> clear_alpha = 1.0;

    environment -> global_variables = hash_create();
    environment -> dynamic_variables = VALUE_NIL;
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
