#include "options.h"
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
    log_level = OPTION_LOG_LEVEL_INITIALIZATION;

    log_section("====INITIALIZE====");
    symbol_initialize();
    Environment *environment = environment_create();
    lisp_initialize(environment);
    log_section("====INITIALIZE-END====")
    return environment;
}
