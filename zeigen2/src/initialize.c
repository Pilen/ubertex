#include "debug.h"
#include "initialize.h"
#include "symbol.h"
#include "lisp.h"
#include "environment.h"
#include "log.h"

Environment *initialize(void) {
    /* static Bool initialized = false; */
    /* if (initialized) { */
    /*     return; */
    /* } */
    /* initialized = true; */
    log_level = LOG_LEVEL_MAX;
    symbol_initialize();
    Environment *environment = environment_create();
    lisp_initialize(environment);
    return environment;
}
