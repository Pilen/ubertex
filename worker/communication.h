#ifndef W_COMMUNICATION_H
#define W_COMMUNICATION_H

#include "vector.h"
#include "lock.h"

void communication_initialize(Unt port);
void communication_add(Unt frame, Value value);
Bool communication_extract(Unt before, Value *result);

#endif
