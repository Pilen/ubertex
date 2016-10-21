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
    while (args.type == CONS) {
        Value arg = NEXT(args);
        if (arg.type == INTEGER) {
            int_sum += arg.val.integer_val;
        } else if (arg.type == FLOAT) {
            real = true;
            float_sum = int_sum;
            float_sum += arg.val.float_val;
            break;
        } else {
            /* TODO: log error */
            return VALUE_ERROR;
        }
    }
    if (real) {
        while (args.type == CONS) {
            Value arg = NEXT(args);
            if (IS_NUMERIC(arg)) {
                float_sum += NUM_VAL(arg);
            } else {
                /* TODO: log error */
                return VALUE_ERROR;
            }
        }
        return VALUE_FLOAT(float_sum);
    } else {
        return VALUE_INTEGER(int_sum);
    }
}

LISP_BUILTIN(minus, "") {
    if (args.type == NIL) {
        return VALUE_INTEGER(0);
    }
    Int int_sum = 0;
    Double float_sum = 0.0;
    Bool real = false;

    Value first = NEXT(args);
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
    if (args.type == NIL) {
        if (real) {
            return VALUE_FLOAT(- float_sum);
        } else {
            return VALUE_INTEGER(- int_sum);
        }
    }

    while (args.type == CONS) {
        Value arg = NEXT(args);
        if (arg.type == INTEGER) {
            int_sum -= arg.val.integer_val;
        } else if (arg.type == FLOAT) {
            float_sum -= arg.val.float_val;
            real = true;
            break;
        } else {
            /* TODO: log error */
            return VALUE_ERROR;
        }
    }
    if (real) {
        while (args.type == CONS) {
            Value arg = NEXT(args);
            if (IS_NUMERIC(arg)) {
                float_sum -= NUM_VAL(arg);
            } else {
                /* TODO: log error */
                return VALUE_ERROR;
            }
        }
        return VALUE_FLOAT(float_sum);
    } else {
        return VALUE_INTEGER(int_sum);
    }
}

LISP_BUILTIN(times, "") {
    Int int_product = 1;
    Double float_product = 1.0;
    Bool real = false;
    while (args.type == CONS) {
        Value arg = NEXT(args);
        if (arg.type == INTEGER) {
            int_product *= arg.val.integer_val;
        } else if (arg.type == FLOAT) {
            real = true;
            float_product = int_product;
            float_product *= arg.val.float_val;
            break;
        } else {
            /* TODO: log error */
            return VALUE_ERROR;
        }
    }
    if (real) {
        while (args.type == CONS) {
            Value arg = NEXT(args);
            if (IS_NUMERIC(arg)) {
                float_product *= NUM_VAL(arg);
            } else {
                /* TODO: log error */
                return VALUE_ERROR;
            }
        }
        return VALUE_FLOAT(float_product);
    } else {
        return VALUE_INTEGER(int_product);
    }
}

LISP_BUILTIN(greater_than, "") {
    if (args.type != CONS) {
        return VALUE_ERROR;
    }

    Value previous = NEXT(args);

    while (args.type == CONS) {
        Value current = NEXT(args);
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
    ENSURE_NOT_EMPTY(args);
    Value angle_v = NEXT(args);
    ENSURE_EMPTY(args);
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
    ENSURE_NOT_EMPTY(args);
    Value angle_v = NEXT(args);
    ENSURE_EMPTY(args);
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
    ENSURE_NOT_EMPTY(args);
    Value first = NEXT(args);
    if (first.type != INTEGER) {
        return VALUE_ERROR;
    }
    if (args.type == CONS) {
        Value second = NEXT(args);
        if (second.type != INTEGER) {
            return VALUE_ERROR;
        }
        lower = first.val.integer_val;
        upper = second.val.integer_val;
    } else {
        lower = 0;
        upper = first.val.integer_val;

    }
    ENSURE_EMPTY(args);
    if (lower >= upper) {
        return VALUE_ERROR;
    }
    return VALUE_INTEGER(random_int(lower, upper));
}
