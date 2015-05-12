
#include <SDL2/SDL.h>
#include "lock.h"
#include "memory.h"
#include "assert.h"

Lock_RW *lock_create_rw() {
    Lock_RW *lock = memory_malloc(sizeof(Lock_RW));
    lock -> readcount = 0;
    lock -> write = SDL_CreateMutex();
    lock -> critical = SDL_CreateMutex();
    lock -> incomming = SDL_CreateMutex();
    return lock;
}

void lock_read_lock(Lock_RW *lock) {
    Int error;
    error = SDL_LockMutex(lock -> incomming); assert(error == 0);
    error = SDL_LockMutex(lock -> critical); assert(error == 0);
    lock -> readcount++;
    if (lock -> readcount == 1) {
        error = SDL_LockMutex(lock -> write); assert(error == 0);
    }
    error = SDL_UnlockMutex(lock -> critical); assert(error == 0);
    error = SDL_UnlockMutex(lock -> incomming); assert(error == 0);
}

void lock_read_unlock(Lock_RW *lock) {
    Int error;
    error = SDL_LockMutex(lock -> critical); assert(error == 0);
    lock -> readcount--;
    if (lock -> readcount== 0) {
        error = SDL_UnlockMutex(lock -> write); assert(error == 0);
    }
    error = SDL_UnlockMutex(lock -> critical); assert(error == 0);

}
void lock_write_lock(Lock_RW *lock) {
    Int error;
    error = SDL_LockMutex(lock -> incomming); assert(error == 0);
    error = SDL_LockMutex(lock -> write); assert(error == 0);
    error = SDL_UnlockMutex(lock -> incomming); assert(error == 0);
}

void lock_write_unlock(Lock_RW *lock) {
    Int error;
    error = SDL_UnlockMutex(lock -> write); assert(error == 0);
}
