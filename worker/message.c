#include "headers.h"

void message_set_handler(Component *component, Value symbol, Value params, Value body, Environment *environment) {
    (void) environment; /* Environment not actually used */
    Value handler = CONS(params, body);
    hash_set(component -> message_handlers, symbol, handler);
}

void message_send(Value recipient, Value message, Environment *environment) {
    if (environment -> messages.type != CONS) {
        environment -> messages = CONS1(CONS(recipient, message));
        environment -> last_message = environment -> messages;
    } else {
        Value new = CONS1(CONS(recipient, message));
        CDR(environment -> last_message) = new;
        environment -> last_message = new;
    }
}

void message_dispatch(Environment *environment) {
    while (environment -> messages.type == CONS) {
        if (loop_abort) {
            return;
        }
        Value pair = NEXT(environment -> messages);
        Value recipient = CAR(pair);
        Value message = CDR(pair);
        if (recipient.type == COMPONENT) {
            /* Send to component */
            Component *component = recipient.val.component_val;
            message_receive(component, message, environment);
        } else if (recipient.type == NIL) {
            /* Send to all */
            Layer *layer = environment -> layers;
            while (layer) {
                Value components = layer -> entries;
                while (components.type == CONS) {
                    Value component_value = NEXT(components);
                    Component *component = component_value.val.component_val;
                    message_receive(component, message, environment);
                }
                layer = layer -> next;
            }
        } else if (recipient.type == SYMBOL) {
            /* Send to all of kind */
            w_assert(false) /* TODO: implement */
        } else {
            w_assert(false);
        }
    }
}

Bool message_receive(Component *component, Value message, Environment *environment) {
    Value symbol = NEXT(message);
    Value handler;
    Bool found = hash_get(component -> message_handlers, symbol, &handler);
    if (!found) {
        /* TODO: log error */
        log_error("Message handler not found");
        return false;
    }
    Value args = message;
    Value params = CAR(handler);
    Value body = CDR(handler);
    Value bindings = VALUE_NIL;
    while (params.type == CONS && args.type == CONS) {
        Value param = NEXT(params);
        Value arg = NEXT(args);
        bindings = CONS(CONS(param, arg), bindings);
    }
    while (params.type == CONS) {
        Value param = NEXT(params);
        bindings = CONS(param, VALUE_ERROR);
        /* TODO: log error better */
        log_warning("Missing argument in message, set");
    }
    if (args.type != NIL) {
        /* TODO: log error better */
        log_warning("Too many arguments for handler, ignored");
    }

    environment -> current_layer = component -> layer -> index;
    environment -> current_component = component;

    environment_bind_variables(component -> local_variables, environment);
    environment_bind_variables(bindings, environment);
    eval(body, environment);
    environment_unbind_variables(environment);
    environment_unbind_variables(environment);
    return true;
}
