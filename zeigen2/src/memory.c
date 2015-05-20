
#include <stdlib.h>
#include <string.h>

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

void *memory_cmalloc_actual(size_t size) {
    void *allocated = memory_malloc_actual(size);
    memset(allocated, 0, size);
    return allocated;

}

void memory_free(void *ptr) {
    free(ptr);
}

void memory_ref_inc(Value value) {
}
void memory_ref_dec(Value value) {

}
