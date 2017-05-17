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

LISP_BUILTIN(division, "") {
    Bool real;
    ENSURE_NOT_EMPTY(args);
    Value args_copy = args;
    while (args_copy.type == CONS) {
        Value arg = NEXT(args_copy);
        if (arg.type == INTEGER) {
            continue;
        } else if (arg.type == FLOAT) {
            real = true;
        } else {
            return VALUE_ERROR;
        }
    }

    Value arg = NEXT(args);
    if (real) {
        Double result = NUM_VAL(arg);
        while (args.type == CONS) {
            arg = NEXT(args);
            result /= NUM_VAL(arg);
        }
        return VALUE_FLOAT(result);
    } else {
        Int result = arg.val.integer_val;
        while (args.type == CONS) {
            arg = NEXT(args);
            result /= arg.val.integer_val;
        }
        return VALUE_INTEGER(result);
    }
}

LISP_BUILTIN(abs, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg = NEXT(args);
    ENSURE_EMPTY(args);
    if (arg.type == INTEGER) {
        Int number = arg.val.integer_val;
        return VALUE_INTEGER(abs(number));
    } else if (arg.type == FLOAT) {
        Double number = arg.val.float_val;
        return VALUE_FLOAT(fabs(number));
    } else {
        return VALUE_ERROR;
    }
}

LISP_BUILTIN(log, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg_v = NEXT(args);
    if (!IS_NUMERIC(arg_v)) {
        return VALUE_ERROR;
    }
    Double arg = NUM_VAL(arg_v);
    if (arg == 0.0) {
        return VALUE_ERROR;
    }

    if (args.type == CONS) {
        Value base_v = NEXT(args);
        ENSURE_EMPTY(args);
        Double base = NUM_VAL(base_v);
        Double result = log(arg) / log(base);
        return VALUE_FLOAT(result);
    } else {
        ENSURE_EMPTY(args);
        Double result = log(arg);
        return VALUE_FLOAT(result);
    }
}

LISP_BUILTIN(log10, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg_v = NEXT(args);
    ENSURE_EMPTY(args);
    if (!IS_NUMERIC(arg_v)) {
        return VALUE_ERROR;
    }
    Double arg = NUM_VAL(arg_v);
    if (arg == 0.0) {
        return VALUE_ERROR;
    }
    Double result = log10(arg);
    return VALUE_FLOAT(result);
}

LISP_BUILTIN(log2, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg_v = NEXT(args);
    ENSURE_EMPTY(args);
    if (!IS_NUMERIC(arg_v)) {
        return VALUE_ERROR;
    }
    Double arg = NUM_VAL(arg_v);
    if (arg == 0.0) {
        return VALUE_ERROR;
    }
    Double result = log2(arg);
    return VALUE_FLOAT(result);
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

LISP_BUILTIN(rad_to_deg, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg_v = NEXT(args);
    ENSURE_EMPTY(args);
    if (!IS_NUMERIC(arg_v)) {
        return VALUE_ERROR;
    }
    Double arg = NUM_VAL(arg_v);
    return VALUE_FLOAT(RAD_TO_DEG(arg));
}

LISP_BUILTIN(deg_to_rad, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg_v = NEXT(args);
    ENSURE_EMPTY(args);
    if (!IS_NUMERIC(arg_v)) {
        return VALUE_ERROR;
    }
    Double arg = NUM_VAL(arg_v);
    return VALUE_FLOAT(DEG_TO_RAD(arg));
}

LISP_BUILTIN(pow, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg1 = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value arg2 = NEXT(args);
    ENSURE_EMPTY(args);

    if (arg1.type == INTEGER && arg2.type == INTEGER) {
        int base = arg1.val.integer_val;
        int exp = arg2.val.integer_val;
        Int result = 1;
        while (exp) {
            if (exp & 1) {
                result *= base;
            }
            exp /= 2;
            base *= base;
        }
        return VALUE_INTEGER(result);
    } else if (IS_NUMERIC(arg1) && IS_NUMERIC(arg2)) {
        double result = pow(NUM_VAL(arg1), NUM_VAL(arg2));
        return VALUE_FLOAT(result);
    } else {
        return VALUE_ERROR;
    }
}

LISP_BUILTIN(sqr, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg = NEXT(args);
    ENSURE_EMPTY(args);
    if (arg.type == INTEGER) {
        int base = arg.val.integer_val;
        return VALUE_INTEGER(base * base);
    } else if (arg.type == FLOAT) {
        Double base = arg.val.float_val;
        return VALUE_FLOAT(base * base);
    } else {
        return VALUE_ERROR;
    }
}

LISP_BUILTIN(sqrt, "") {
    ENSURE_NOT_EMPTY(args);
    Value arg = NEXT(args);
    ENSURE_EMPTY(args);
    if (IS_NUMERIC(arg)) {
        Double base = NUM_VAL(arg);
        if (base < 0.0) {
            return VALUE_ERROR;
        }
        Double result = sqrt(base);
        return VALUE_FLOAT(result);
    }
    return VALUE_ERROR;
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


LISP_BUILTIN(floor, "") {
    ENSURE_NOT_EMPTY(args);
    Value val = NEXT(args);
    ENSURE_EMPTY(args);

    if (val.type == INTEGER) {
        return val;
    }
    if (val.type == FLOAT) {
        Int floored = (Int) floor(val.val.float_val);
        return VALUE_INTEGER(floored);
    }
    return VALUE_ERROR;
}

LISP_BUILTIN(ceil, "") {
    ENSURE_NOT_EMPTY(args);
    Value val = NEXT(args);
    ENSURE_EMPTY(args);

    if (val.type == INTEGER) {
        return val;
    }
    if (val.type == FLOAT) {
        Int ceiled = (Int) ceil(val.val.float_val);
        return VALUE_INTEGER(ceiled);
    }
    return VALUE_ERROR;
}

LISP_BUILTIN(round, "") {
    ENSURE_NOT_EMPTY(args);
    Value val = NEXT(args);
    ENSURE_EMPTY(args);

    if (val.type == INTEGER) {
        return val;
    }
    if (val.type == FLOAT) {
        Int rounded = (Int) round(val.val.float_val);
        return VALUE_INTEGER(rounded);
    }
    return VALUE_ERROR;
}
