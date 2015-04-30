#include "../debug.h"
#include "../types.h"
#include "../eval.h"
#include "../list.h"

LISP_BUILTIN(plus, "") {
    Int int_sum = 0;
    Double float_sum = 0;

    Bool real = false;
    for (Unt i = 1; i < args -> length; i++) {
        Value arg = LIST_GET_UNSAFE(args, i);
        switch (arg.type) {
        case INTEGER:
            int_sum += arg.val.integer_val;
            break;
        case FLOAT:
            float_sum += arg.val.float_val;
            real = true;
            break;
        default:
            /* TODO: log error */
            return VALUE_ERROR;
        }
    }
    if (real) {
        return VALUE_FLOAT(float_sum + int_sum);
    } else {
        return VALUE_INTEGER(int_sum);
    }
}

LISP_BUILTIN(minus, "") {
    if (args -> length == 1) {
        return VALUE_INTEGER(0);
    }
    Int int_sum = 0;
    Double float_sum = 0;

    Bool real = false;

    Value first = LIST_GET_UNSAFE(args, 1);
    switch (first.type) {
    case INTEGER:
        int_sum = first.val.integer_val;
        break;
    case FLOAT:
        float_sum = first.val.float_val;
        real = true;
        break;
    default:
        /* TODO: log error */
        return VALUE_ERROR;
    }
    if (args -> length == 2) {
        if (real) {
            return VALUE_FLOAT(- float_sum);
        } else {
            return VALUE_INTEGER(- int_sum);
        }

    }

    for (Unt i = 2; i < args -> length; i++) {
        Value arg = LIST_GET_UNSAFE(args, i);
        switch (arg.type) {
        case INTEGER:
            int_sum -= arg.val.integer_val;
            break;
        case FLOAT:
            float_sum -= arg.val.float_val;
            real = true;
            break;
        default:
            /* TODO: log error */
            return VALUE_ERROR;
        }
    }
    if (real) {
        return VALUE_FLOAT(float_sum + int_sum);
    } else {
        return VALUE_INTEGER(int_sum);
    }
}
