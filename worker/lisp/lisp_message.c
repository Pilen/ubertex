#include "../headers.h"

LISP_BUILTIN(send, "") {
    ENSURE_NOT_EMPTY(args);
    Value recipient = NEXT(args);
    if (recipient.type != COMPONENT && recipient.type != NIL && recipient.type != SYMBOL) {
        return VALUE_ERROR;
    }
    ENSURE_NOT_EMPTY(args);
    message_send(recipient, args, environment);
    return VALUE_NIL;
}

LISP_BUILTIN(broadcast, "") {
    ENSURE_NOT_EMPTY(args);
    message_send(VALUE_NIL, args, environment);
    return VALUE_NIL;
}

LISP_BUILTIN(receive, "") {
    ENSURE_NOT_EMPTY(args);
    Value params = NEXT(args);
    ENSURE_NOT_EMPTY(params);
    Value symbol = NEXT(params);
    Value body = VALUE_NIL;
    if (args.type == CONS) {
        if (CDR(args).type == NIL) {
            body = CAR(args);
        } else {
            body = CONS(symbols_progn, args);
        }
    }
    if (!environment -> current_component) {
        /* TODO: log error */
        return VALUE_ERROR;
    }
    message_set_handler(environment -> current_component, symbol, params, body, environment);
    return VALUE_NIL;
}
