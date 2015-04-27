#include "types.h"
#include "environment.h"
#include "hash.h"
#include "memory.h"

Environment *environment_create(void) {
    Environment *environment = z_malloc(sizeof(Environment));
    environment -> variables = hash_create();
    environment -> functions = hash_create();
    return environment;
}
