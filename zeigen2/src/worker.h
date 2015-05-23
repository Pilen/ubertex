#ifndef Z_WORKER_H
#define Z_WORKER_H

#include "types.h"
#include "environment.h"

void worker_loop(Environment *environment);
Bool worker_unfreeze;

#endif
