#ifndef Z_MEMORY_H
#define Z_MEMORY_H

#include <stdlib.h>

#include "options.h"
#include "types.h"
#include "log.h"

#if OPTION_DEBUG_MEMORY
#define z_malloc(size) (log_malloc(size), memory_malloc_actual(size))
#define z_cmalloc(size) (log_malloc(size), memory_cleared_malloc_actual(size))
#define z_calloc(amount, size) (log_calloc(amount, size), memory_calloc_actual(amount, size))
#else
#define z_malloc(size) memory_malloc_actual(size)
#define z_calloc(amount, size) memory_calloc_actual(amount, size)
#endif

void *memory_malloc_actual(size_t size);
void *memory_cleared_malloc_actual(size_t size);
void memory_free(void *ptr);
void *memory_calloc_actual(size_t amount, size_t size);

/* #define INC_REF(item) (item -> refcount++) */
/* #define DEC_REF(item) (item -> refcount--) */

void z_ref_inc(Value value);
void z_ref_dec(Value value);

#endif
