#ifndef W_LOCK_H
#define W_LOCK_H

#include <SDL2/SDL.h>
#include "types.h"

typedef SDL_mutex Mutex;

typedef struct {
    Unt readcount;
    SDL_mutex *write;
    SDL_mutex *critical;
    SDL_mutex *incomming;
} Lock_RW;


Lock_RW *lock_rw_create();
void lock_read_lock(Lock_RW *lock);
void lock_read_unlock(Lock_RW *lock);
void lock_write_lock(Lock_RW *lock);
void lock_write_unlock(Lock_RW *lock);

Mutex *mutex_create();
void mutex_lock(Mutex *lock);
void mutex_unlock(Mutex *lock);
Bool mutex_trylock(Mutex *lock);

#endif
