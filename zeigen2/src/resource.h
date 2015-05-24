#ifndef Z_RESOURCE_H
#define Z_RESOURCE_H

#include "environment.h"

size_t resource_size_threshold;

void resource_initialize(void);
Value resource_get(Environment *environment, Value skeleton);

Unt resource_flush_cache();


#endif
