#include "../math.h"
#include "../debug.h"
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../symbol.h"


LISP_BUILTIN(plus, "") {
    Int int_sum = 0;
    Double float_sum = 0.0;

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
    Double float_sum = 0.0;

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

LISP_BUILTIN(times, "") {
    Int int_product = 1;
    Double float_product = 1.0;

    Bool real = false;
    for (Unt i = 1; i < args -> length; i++) {
        Value arg = LIST_GET_UNSAFE(args, i);
        switch (arg.type) {
        case INTEGER:
            int_product *= arg.val.integer_val;
            break;
        case FLOAT:
            float_product *= arg.val.float_val;
            real = true;
            break;
        default:
            /* TODO: log error */
            return VALUE_ERROR;
        }
    }
    if (real) {
        return VALUE_FLOAT(float_product + int_product);
    } else {
        return VALUE_INTEGER(int_product);
    }
}

LISP_BUILTIN(greater_than, "") {
    if (args -> length < 1) {
        return VALUE_ERROR;
    }

    Value previous = LIST_GET_UNSAFE(args, 1);

    for (Unt i = 2; i < args -> length; i++) {
        Value current = LIST_GET_UNSAFE(args, i);
        switch (previous.type) {
        case INTEGER:
            switch (current.type) {
            case INTEGER:
                if (previous.val.integer_val > current.val.integer_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            case FLOAT:
                if (previous.val.integer_val > current.val.float_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            default:
                return VALUE_ERROR;
            }
        case FLOAT:
            switch (current.type) {
            case INTEGER:
                if (previous.val.float_val > current.val.integer_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            case FLOAT:
                if (previous.val.float_val > current.val.float_val) {
                    previous = current;
                    continue;
                } else {
                    return VALUE_NIL;
                }
            default:
                return VALUE_ERROR;
            }
        default:
            return VALUE_ERROR;
        }
    }
    return symbols_t;
}


LISP_BUILTIN(sin, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }

    Value angle_v = LIST_GET_UNSAFE(args, 1);
    Double angle;
    switch (angle_v.type) {
    case INTEGER:
        angle = angle_v.val.integer_val;
        break;
    case FLOAT:
        angle = angle_v.val.float_val;
        break;
    default:
        return VALUE_ERROR;
    }
    Double result = RAD_TO_DEG(sin(DEG_TO_RAD(angle)));
    return VALUE_FLOAT(result);
}

LISP_BUILTIN(cos, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }

    Value angle_v = LIST_GET_UNSAFE(args, 1);
    Double angle;
    switch (angle_v.type) {
    case INTEGER:
        angle = angle_v.val.integer_val;
        break;
    case FLOAT:
        angle = angle_v.val.float_val;
        break;
    default:
        return VALUE_ERROR;
    }
    Double result = RAD_TO_DEG(cos(DEG_TO_RAD(angle)));
    return VALUE_FLOAT(result);
}

LISP_BUILTIN(randint, "") {
    Int lower;
    Int upper;

    if (args -> length == 3) {
        Value lower_value = LIST_GET_UNSAFE(args, 1);
        Value upper_value = LIST_GET_UNSAFE(args, 2);
        if (lower_value.type != INTEGER || upper_value.type != INTEGER) {
            return VALUE_ERROR;
        }
        lower = lower_value.val.integer_val;
        upper = upper_value.val.integer_val;
    } else if (args -> length == 2) {
        Value upper_value = LIST_GET_UNSAFE(args, 2);
        if (upper_value.type != INTEGER) {
            return VALUE_ERROR;
        }
        lower = 0;
        upper = upper_value.val.integer_val;
    } else {
        return VALUE_ERROR;
    }

    return VALUE_INTEGER(random_int(lower, upper));
}
