#ifndef Z_MEMORY_H
#define Z_MEMORY_H

#include <stdlib.h>

#include "types.h"


void *z_malloc(size_t size);
void z_free(void *ptr);
void *z_calloc(size_t amount, size_t size);

Value z_copy(Value value);


/* #define INC_REF(item) (item -> refcount++) */
/* #define DEC_REF(item) (item -> refcount--) */

void z_ref_inc(Value value);
void z_ref_dec(Value value);

#endif
