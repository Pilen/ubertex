#ifndef W_MEMORY_H
#define W_MEMORY_H

#include <stdlib.h>

#define GC_THREADS
#include <signal.h>
/* #include <linux/signal.h> */
#include <gc.h>

#if OPTION_DEBUG_MEMORY
#define memory_malloc(size) (log_malloc(size), memory_malloc_actual(size))
#else
#define memory_malloc(size) (memory_malloc_actual(size))
#endif

#define NEW(type) memory_malloc(sizeof(type))
#define NEW_BUFFER(type, count) memory_malloc(sizeof(type) * count)

void memory_initialize(void);
void memory_update(void);
void memory_register_thread(void);
size_t memory_estimate_available(void);

void *memory_malloc_actual(size_t size);
void memory_free(void *ptr);

/* #define INC_REF(item) (item -> refcount++) */
/* #define DEC_REF(item) (item -> refcount--) */

void memory_ref_inc(Value value);
void memory_ref_dec(Value value);

#endif
