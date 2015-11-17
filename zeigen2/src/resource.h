#ifndef Z_RESOURCE_H
#define Z_RESOURCE_H

#include "environment.h"

size_t resource_size_threshold;

void resource_initialize(void);
Value resource_get(Environment *environment, Value skeleton);

Unt resource_shrink_cache(void);
Unt resource_dump_entire_cache(void);
Unt resource_dump_dirty_cache(void);


/**
 * Resources are
 * Image
 * PDF
 * Soundsample
 * Text
 */

#endif
