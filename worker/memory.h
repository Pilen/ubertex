#ifndef W_MEMORY_H
#define W_MEMORY_H

#include <stdlib.h>

#define GC_THREADS
#include <signal.h>
/* #include <linux/signal.h> */
#include <gc.h>

#if OPTION_DEBUG_MEMORY
#define memory_malloc(size) (log_malloc(size), memory_malloc_actual(size))
#define memory_cmalloc(size) (log_malloc(size), memory_cmalloc_actual(size))
#else
#define memory_malloc(size) (memory_malloc_actual(size))
#define memory_cmalloc(size) (memory_cmalloc_actual(size))
#endif

void memory_initialize(void);
void memory_update(void);
void memory_register_thread(void);
size_t memory_estimate_available(void);

void *memory_malloc_actual(size_t size);
void *memory_cmalloc_actual(size_t size);
void memory_free(void *ptr);

/* #define INC_REF(item) (item -> refcount++) */
/* #define DEC_REF(item) (item -> refcount--) */

void memory_ref_inc(Value value);
void memory_ref_dec(Value value);

#endif
