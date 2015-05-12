#ifndef Z_LOCK_H
#define Z_LOCK_H

#include "types.h"

typedef struct {
    Unt readcount;
    SDL_mutex *write;
    SDL_mutex *critical;
    SDL_mutex *incomming;
} Lock_RW;

Lock_RW *lock_create_rw();
void lock_read_lock(Lock_RW *lock);
void lock_read_unlock(Lock_RW *lock);
void lock_write_lock(Lock_RW *lock);
void lock_write_unlock(Lock_RW *lock);

#endif
