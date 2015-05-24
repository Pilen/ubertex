#ifndef Z_IMAGE_H
#define Z_IMAGE_H

#include <SDL2/SDL.h>
#include "types.h"
#include "environment.h"

struct Image_s {
    Unt refcount;
    float score;
    Value path; /* A Value so we can allow different sources than files */
    Unt size; /* approximate */
    SDL_Texture *texture;
};

SDL_Texture *image_get_texture_from_file(Environment *environment, Value filename);

Bool image_create(Environment *environment, Value skeleton, Unt initial_score, Unt *size);

#endif
