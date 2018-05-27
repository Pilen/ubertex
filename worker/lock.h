#ifndef W_LOCK_H
#define W_LOCK_H

#include <SDL2/SDL.h>

typedef SDL_mutex Mutex;

typedef struct {
    Unt readcount;
    SDL_mutex *write;
    SDL_mutex *critical;
    SDL_mutex *incomming;
} Lock_RW;

typedef SDL_atomic_t Flag;
/* Hoists/lower the flag, returns the current value of the flag */
#define flag_hoist(flag) (SDL_AtomicSet(&(flag), 1))
#define flag_hoist_to(flag, value) (SDL_AtomicSet(&(flag), (value)))
#define flag_lower(flag) (SDL_AtomicSet(&(flag), 0))
#define flag_is_up(flag) (SDL_AtomicGet(&(flag)))

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
