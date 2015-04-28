
#include <stdlib.h>

#include "memory.h"
#include "log.h"
#include "debug.h"

void *memory_malloc_actual(size_t size) {
    static Unt i = 0;
    /* debug("i=%d\t size = %td", i, size); */
    i++;
    void *allocated = malloc(size);
    if (allocated == NULL) {
        log_fatal("Internal failure in %s\nUnable to allocate additional memory!", __func__);
    }
    return allocated;
}

void memory_free(void *ptr) {
    free(ptr);
}

void *memory_calloc_actual(size_t amount, size_t size) {
    void *allocated = calloc(amount, size);
    if (allocated == NULL) {
        log_fatal("Internal failure in %s\nUnable to allocate additional memory!", __func__);
    }
    return allocated;

}

void z_ref_inc(Value value) {
}
void z_ref_dec(Value value) {

}
