#ifndef Z_RESOURCE_H
#define Z_RESOURCE_H

#include <SDL2/SDL.h>
#include "environment.h"

typedef enum {
    RESOURCE_TEXTURE,
    /* RESOURCE_SOUND, */
    /* RESOURCE_FONT */
} Resource_type;

struct Resource_s {
    Unt refcount;
    float score;
    Resource_type type;
    union {
        SDL_Texture *texture_val;
    } val;
};

void resource_initialize(void);

SDL_Texture *resource_get_image(Environment *environment, Value filename);
Unt resource_flush_cache(Environment *environment, Unt amount);


#endif
