
#include <stdlib.h>

#include "memory.h"
#include "log.h"
#include "debug.h"


void *memory_malloc_actual(size_t size) {
    void *allocated = malloc(size);
    if (!allocated) {
        log_fatal("Internal failure in %s\nUnable to allocate additional memory! %zd bytes requested", __func__, size);
    }
    return allocated;
}

void *memory_malloc_cleared_actual(size_t size) {
    void *allocated = calloc(1, size);
    if (!allocated) {
        log_fatal("Internal failure in %s\nUnable to allocate additional memory! %zd bytes requested", __func__, size);
    }
    return allocated;

}

void *memory_calloc_actual(size_t amount, size_t size) {
    void *allocated = calloc(amount, size);
    if (!allocated) {
        log_fatal("Internal failure in %s\nUnable to allocate additional memory! %zd bytes requested", __func__, amount * size);
    }
    return allocated;

}

void memory_free(void *ptr) {
    free(ptr);
}

void memory_ref_inc(Value value) {
}
void memory_ref_dec(Value value) {

}
