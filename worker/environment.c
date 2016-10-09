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
    environment -> base_texture = NULL;
    environment -> cairo_surface = NULL;
    environment -> cairo = NULL;

    environment -> width = 0;
    environment -> height = 0;

    environment -> component_next_update = VALUE_NIL; /* Not set */
    environment -> component_next_update_args = VALUE_NIL; /* list */
    environment -> component_next_post = VALUE_NIL; /* Not set */
    environment -> component_next_post_args = VALUE_NIL; /* list */

    environment -> clear_red = 0.0;
    environment -> clear_green = 0.0;
    environment -> clear_blue = 0.0;
    environment -> clear_alpha = 1.0;

    environment -> variables = hash_create();
    environment -> functions = hash_create();

    environment -> call_stack = VALUE_NIL; /* list */
    return environment;
}
