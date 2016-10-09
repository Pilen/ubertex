
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../resource.h"
#include "../graphics.h"

LISP_BUILTIN(next_update, "") {
    /* All the args are already evaluated in the call to next_update */
    ENSURE_NOT_EMPTY(args);
    Value function = NEXT(args);
    environment -> component_next_update = function;
    if (function.type == ERROR) {
        return VALUE_ERROR;
    }
    environment -> component_next_update_args = args;
    return VALUE_NIL;
}

LISP_BUILTIN(next_post, "") {
    /* All the args are already evaluated in the call to next_update */
    ENSURE_NOT_EMPTY(args);
    Value function = NEXT(args);
    environment -> component_next_post = function;
    if (function.type == ERROR) {
        return VALUE_ERROR;
    }
    environment -> component_next_post_args = args;
    return VALUE_NIL;
}
