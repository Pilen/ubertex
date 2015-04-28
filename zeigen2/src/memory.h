#ifndef Z_MEMORY_H
#define Z_MEMORY_H

#include <stdlib.h>

#include "types.h"

/* #define DEBUG_MEMORY */
#ifdef DEBUG_MEMORY
#define z_malloc(size) (printf("MALLOC: %s:%d:\t %s\t\t\t %s = %td\n", __FILE__, __LINE__, __func__, #size, size), memory_malloc_actual(size))
/* #define z_calloc(amount, size) (printf("CALLOC:\t\t %s:%d: n = %d \t%s = %td\n", __FILE__, __LINE__, amount, #size, size), memory_calloc_actual(amount, size)) */
#define z_calloc(amount, size) memory_calloc_actual(amount, size)
#else
#define z_malloc(size) memory_malloc_actual(size)
#define z_calloc(amount, size) memory_calloc_actual(amount, size)
#endif

void *memory_malloc_actual(size_t size);
void memory_free(void *ptr);
void *memory_calloc_actual(size_t amount, size_t size);

/* #define INC_REF(item) (item -> refcount++) */
/* #define DEC_REF(item) (item -> refcount--) */

void z_ref_inc(Value value);
void z_ref_dec(Value value);

#endif
