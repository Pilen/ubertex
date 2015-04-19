
#include <stdlib.h>

#include "zmemory.h"


void *z_malloc(size_t size) {
    void *allocated = malloc(size);
    if (allocated == NULL) {
        /* TODO: log error */
    }
    return allocated;
}

void free(void *ptr) {
    free(ptr);
}

void *z_calloc(size_t amount, size_t size) {
    void *allocated = calloc(amount, size);
    if (allocated == NULL) {
        /* TODO: log error */
    }
    return allocated;

}

void z_ref_inc(Value value) {
}
void z_ref_dec(Value value) {

}
