
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
    if (args -> length < 2) {
        return VALUE_ERROR;
    }

    Value function = LIST_GET_UNSAFE(args, 1);
    environment -> component_next_update = function;

    List *update_args = list_create(round_up_to_power_of_2(args -> length - 1));
    for (Unt i = 1; i < args -> length; i++) {
        Value arg = LIST_GET_UNSAFE(args, i);
        list_push_back(update_args, arg);
    }
    if (function.type == ERROR) {
        return VALUE_ERROR;
    }
    environment -> component_next_update_args = update_args;
    return VALUE_NIL;
}

LISP_BUILTIN(next_post, "") {
    if (args -> length < 2) {
        return VALUE_ERROR;
    }

    Value function = LIST_GET_UNSAFE(args, 1);
    environment -> component_next_post = function;

    List *post_args = list_create(round_up_to_power_of_2(args -> length - 1));
    for (Unt i = 1; i < args -> length; i++) {
        Value arg = LIST_GET_UNSAFE(args, i);
        list_push_back(post_args, arg);
    }
    if (function.type == ERROR) {
        return VALUE_ERROR;
    }
    environment -> component_next_post_args = post_args;
    return VALUE_NIL;
}
