
#include "debug.h"
#include "types.h"
#include "list.h"
#include "memory.h"
#include "assert.h"

Cons *list_cons(Value a, Value b) {
    Cons *cons = memory_malloc(sizeof(Cons));
    cons -> car = a;
    cons -> cdr = b;
    return cons;
}

Value list_length(Value list) {
    Unt length = 0;
    while (true) {
        if (list.type == NIL) {
            return VALUE_INTEGER(length);
        }
        if (list.type != CONS) {
            return VALUE_ERROR;
        }
        length++;
        list = CDR(list);
    }
};

Value list_nth(Value list, Unt n) {
    while (n > 0) {
        if (list.type == CONS) {
            NEXT(list);
            n--;
        } else {
            return VALUE_ERROR;
        }
    }
    if (list.type != CONS) {
        return VALUE_ERROR;
    }
    return CAR(list);

}
Value list_reverse(Value list) {
    /* Inplace. Remember always to assign the returned value (do not rely only on modification) */
    /* a -> b -> c -> nil */
    Value rest = list;
    Value result = VALUE_NIL;

    while (true) {
        if (rest.type == NIL) {
            break;
        }
        if (rest.type != CONS) {
            log_error("Reversing an improper list");
            return VALUE_ERROR;
        }
        Value current = rest;
        rest = CDR(rest);
        CDR(current) = result;
        result = current;
    }
    return result;
}

Value list_copy(Value list) {
    if (list.type == NIL) {
        return VALUE_NIL;
    }
    Value result = CONS1(VALUE_NIL);
    Value last = result;
    while (list.type == CONS) {
        CAR(last) = NEXT(list)
        if (list.type == CONS) {
            CDR(last) = CONS1(VALUE_NIL);
            last = CDR(last);
        } else {
            CDR(last) = list;
            break;
        }
    }
    ENSURE_EMPTY(list);
    return result;
}

/* /\* This one is similar to the above, which is better? Benchmark *\/ */
/* Value list_copy(Value list) { */
/*     Value temp = VALUE_NIL; */
/*     while (list.type == CONS) { */
/*         temp = CONS(CAR(list), temp); */
/*         list = CDR(list); */
/*     } */
/*     ENSURE_EMPTY(args); */
/*     Value result = VALUE_NIL; */
/*     while (list.type == CONS) { */
/*         result = CONS(CAR(temp), result); */
/*         temp = CDR(temp); */
/*     } */
/*     return result; */
/* } */

void list_destroy(Value list) {
};
