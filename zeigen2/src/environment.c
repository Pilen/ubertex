#include "types.h"
#include "environment.h"
#include "hash.h"
#include "memory.h"
#include "list.h"

/* Note: decide if this function should just be located in initialize.c */
Environment *environment_create(void) {
    Environment *environment = memory_malloc(sizeof(Environment));
    environment -> window = NULL;
    environment -> renderer = NULL;

    environment -> component_next_update = VALUE_NIL;
    environment -> component_next_update_args = list_create_empty();

    environment -> setting_clear_red = 0;
    environment -> setting_clear_green = 0;
    environment -> setting_clear_blue = 0;
    environment -> setting_clear_alpha = 255;

    environment -> variables = hash_create();
    environment -> functions = hash_create();

    environment -> call_stack = list_create_empty();
    return environment;
}
