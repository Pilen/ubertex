#ifndef W_MESSAGE_H
#define W_MESSAGE_H

#include "types.h"
#include "environment.h"

void message_set_handler(Component *component, Value symbol, Value params, Value body, Environment *environment);
void message_send(Value recipient, Value message, Environment *environment);
void message_dispatch(Environment *environment);
Bool message_receive(Component *component, Value message, Environment *environment);



#endif
