#ifndef Z_WORKER_H
#define Z_WORKER_H

#include "types.h"
#include "environment.h"

void worker_loop(Environment *environment);
/* FLAGS */
/* TODO: decide if this should be placed somewhere else */
Bool worker_abort;
Bool worker_blank;
Bool flush_dirty_cache;

#endif
