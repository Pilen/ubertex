#ifndef W_LOOP_H
#define W_LOOP_H

void loop(Environment *environment);
/* FLAGS */
/* TODO: decide if this should be placed somewhere else */
Flag loop_abort;
Flag loop_blank;
Flag flush_dirty_cache;
Flag flush_entire_cache;
Flag loop_resync; /* Also act as the new seed */

#endif
