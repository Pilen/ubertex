#ifndef Z_COMMUNICATION_H
#define Z_COMMUNICATION_H

#include "list.h"
#include "lock.h"

List *communication_parsed_queue;
Mutex *communication_parsed_queue_lock;
void communication_initialize(Unt port);

#endif
