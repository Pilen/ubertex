#ifndef W_RESOURCE_H
#define W_RESOURCE_H

#include "environment.h"
#include "lock.h"

size_t resource_size_threshold;

Hash *resource_cache;
Lock_RW *resource_cache_lock;
Vector *resource_vector; /* The resource_cache_lock must be held while using this */
size_t resource_total_size;

void resource_initialize(void);
Value resource_get(Environment *environment, Value skeleton);

Unt resource_shrink_cache(void);
Unt resource_flush_entire_cache(void);
Unt resource_flush_dirty_cache(void);

Unt resource_destroy(Value resource);

/**
 * Resources are
 * Image
 * PDF
 * Soundsample
 */

#endif
