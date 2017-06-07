#ifndef W_LOOP_H
#define W_LOOP_H

void loop(Environment *environment);
/* FLAGS */
/* TODO: decide if this should be placed somewhere else */
volatile Bool loop_abort;
volatile Bool loop_blank;
volatile Bool flush_dirty_cache;
volatile Bool flush_entire_cache;
volatile Bool loop_resync;
volatile Unt loop_new_seed;

#endif
