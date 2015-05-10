#ifndef Z_MEMORY_H
#define Z_MEMORY_H

#include <stdlib.h>

#include "options.h"
#include "types.h"
#include "log.h"

#if OPTION_DEBUG_MEMORY
#define memory_malloc(size) (log_malloc(size), memory_malloc_actual(size))
#define memory_cmalloc(size) (log_malloc(size), memory_malloc_cleared_actual(size))
#define memory_calloc(amount, size) (log_calloc(amount, size), memory_calloc_actual(amount, size))
#else
#define memory_malloc(size) memory_malloc_actual(size)
#define memory_cmalloc(size) memory_malloc_cleared_actual(size)
#define memory_calloc(amount, size) memory_calloc_actual(amount, size)
#endif

void *memory_malloc_actual(size_t size);
void *memory_malloc_cleared_actual(size_t size);
void *memory_calloc_actual(size_t amount, size_t size);
void memory_free(void *ptr);

/* #define INC_REF(item) (item -> refcount++) */
/* #define DEC_REF(item) (item -> refcount--) */

void memory_ref_inc(Value value);
void memory_ref_dec(Value value);

#endif
