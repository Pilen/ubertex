#include "../headers.h"

LISP_BUILTIN(list, "") {
    /* Already evaluated */
    return args;
}

LISP_BUILTIN(cons, "") {
    ENSURE_NOT_EMPTY(args);
    Value car = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value cdr = NEXT(args);
    return CONS(car, cdr);
}

LISP_BUILTIN(car, "") {
    ENSURE_NOT_EMPTY(args);
    Value list = NEXT(args);
    if (list.type != CONS) {
        return VALUE_ERROR;
    }
    return CAR(list);
}

LISP_BUILTIN(cdr, "") {
    ENSURE_NOT_EMPTY(args);
    Value list = NEXT(args);
    if (list.type != CONS) {
        return VALUE_ERROR;
    }
    return CDR(list);
}

LISP_BUILTIN(setcar, "") {
    ENSURE_NOT_EMPTY(args);
    Value cell = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value new_car = NEXT(args);
    ENSURE_EMPTY(args);

    if (cell.type != CONS) {
        return VALUE_ERROR;
    }

    cell.val.cons_val -> car = new_car;
    return new_car;
}

LISP_BUILTIN(setcdr, "") {
    ENSURE_NOT_EMPTY(args);
    Value cell = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value new_cdr = NEXT(args);
    ENSURE_EMPTY(args);

    if (cell.type != CONS) {
        return VALUE_ERROR;
    }

    cell.val.cons_val -> cdr = new_cdr;
    return new_cdr;
}


/* LISP_BUILTIN(nth, "") { */
/*     if (args -> length != 3) { */
/*         return VALUE_ERROR; */
/*     } */

/*     Value value1 = LIST_GET_UNSAFE(args, 1); */
/*     Value value2 = LIST_GET_UNSAFE(args, 2); */
/*     Int n; */
/*     List *list; */
/*     if (value1.type == INT && value2.type == LIST) { */
/*         n = value1.val.int_val; */
/*         list = value2.val.list_val; */
/*     } else if (value1.type == LIST && value2.type == INT) { */
/*         list = value1.val.list_val; */
/*         n = value2.val.int_val; */
/*     } else { */
/*         return VALUE_ERROR; */
/*     } */
/*     return list_get(list, n); */
/* } */

/* LISP_BUILTIN(set-nth, "") { */
/*     if (args -> length != 4) { */
/*         return VALUE_ERROR; */
/*     } */

/*     Value value1 = LIST_GET_UNSAFE(args, 1); */
/*     Value value2 = LIST_GET_UNSAFE(args, 2); */
/*     Value value3 = LIST_GET_UNSAFE(args, 3); */
/*     Int n; */
/*     List *list; */
/*     if (value1.type == INT && value2.type == LIST) { */
/*         n = value1.val.int_val; */
/*         list = value2.val.list_val; */
/*     } else if (value1.type == LIST && value2.type == INT) { */
/*         list = value1.val.list_val; */
/*         n = value2.val.int_val; */
/*     } else { */
/*         return VALUE_ERROR; */
/*     } */
/*     return list_get(list, n, value3); */
/* } */

/* LISP_BUILTIN(push_front, "") { */
/*     if (args -> length != 3) { */
/*         return VALUE_ERROR; */
/*     } */
/*     Value element = LIST_GET_UNSAFE(args, 1); */
/*     Value list_value = LIST_GET_UNSAFE(args, 2); */
/*     if (list_value.type != LIST) { */
/*         return VALUE_ERROR; */
/*     } */
/*     List *list = list_value.val.list_value; */
/*     list_push_front(list, element); */
/* } */

/* LISP_BUILTIN(push_back, "") { */
/*     if (args -> length != 3) { */
/*         return VALUE_ERROR; */
/*     } */
/*     Value element = LIST_GET_UNSAFE(args, 1); */
/*     Value list_value = LIST_GET_UNSAFE(args, 2); */
/*     if (list_value.type != LIST) { */
/*         return VALUE_ERROR; */
/*     } */
/*     List *list = list_value.val.list_value; */
/*     list_push_front(list, element); */
/* } */

/* LISP_BUILTIN(length, "") { */
/*     if (args -> length != 2) { */
/*         return VALUE_ERROR; */
/*     } */
/*     Value value = LIST_GET_UNSAFE(args, 1); */
/*     if (value.type == LIST) { */
/*         List *list = value.val.list_val; */
/*         return list_length(list); */
/*     } */
/*     return VALUE_ERROR; */
/* } */
