#ifndef Z_RESOURCE_H
#define Z_RESOURCE_H

#include <SDL2/SDL.h>
#include "environment.h"

typedef enum {
    RESOURCE_TEXTURE,
    /* RESOURCE_SOUND, */
    /* RESOURCE_FONT */
} Resource_type;

struct Image_s {
    Unt refcount;
    float score;
    Value path; /* A Value so we can allow different sources than files */
    Unt size; /* approximate */
    SDL_Texture *texture;
};

size_t resource_size_threshold;

void resource_initialize(void);

SDL_Texture *resource_image(Environment *environment, Value filename);
Unt resource_flush_cache();


#endif