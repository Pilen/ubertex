#ifndef Z_LOOP_H
#define Z_LOOP_H

#include "types.h"
#include "environment.h"

void loop_loop(Environment *environment);
/* FLAGS */
/* TODO: decide if this should be placed somewhere else */
Bool loop_abort;
Bool loop_blank;
Bool flush_dirty_cache;
Bool flush_entire_cache;

#endif
