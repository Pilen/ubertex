#include "types.h"
#include "environment.h"
#include "hash.h"
#include "memory.h"

/* Note: decide if this function should just be located in initialize.c */
Environment *environment_create(void) {
    Environment *environment = memory_malloc(sizeof(Environment));
    environment -> variables = hash_create();
    environment -> functions = hash_create();

    environment -> call_stack = list_create_empty();
    return environment;
}
