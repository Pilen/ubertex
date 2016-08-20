
#include <SDL2/SDL.h>
#include "lock.h"
#include "memory.h"
#include "assert.h"

Lock_RW *lock_rw_create() {
    Lock_RW *lock = memory_malloc(sizeof(Lock_RW));
    lock -> readcount = 0;
    lock -> write = SDL_CreateMutex();
    lock -> critical = SDL_CreateMutex();
    lock -> incomming = SDL_CreateMutex();
    return lock;
}

void lock_read_lock(Lock_RW *lock) {
    mutex_lock(lock -> incomming);
    mutex_lock(lock -> critical);
    lock -> readcount++;
    if (lock -> readcount == 1) {
        mutex_lock(lock -> write);
    }
    mutex_unlock(lock -> critical);
    mutex_unlock(lock -> incomming);
}

void lock_read_unlock(Lock_RW *lock) {
    mutex_lock(lock -> critical);
    lock -> readcount--;
    if (lock -> readcount== 0) {
        mutex_unlock(lock -> write);
    }
    mutex_unlock(lock -> critical);
}

void lock_write_lock(Lock_RW *lock) {
    mutex_lock(lock -> incomming);
    mutex_lock(lock -> write);
    mutex_unlock(lock -> incomming);
}

void lock_write_unlock(Lock_RW *lock) {
    mutex_unlock(lock -> write);
}


Mutex *mutex_create() {
    Mutex *mutex = SDL_CreateMutex();
    w_assert(mutex);
    return mutex;
}
void mutex_lock(Mutex *lock) {
    Int error = SDL_LockMutex(lock);
    w_assert(error == 0);
}
void mutex_unlock(Mutex *lock) {
    Int error = SDL_UnlockMutex(lock);
    w_assert(error == 0);
}
Bool mutex_trylock(Mutex *lock) {
    Int status = SDL_TryLockMutex(lock);
    if (status == 0) {
        return true;
    } else if (status == SDL_MUTEX_TIMEDOUT) {
        return false;
    }
    w_assert(false);
    return false;
}
