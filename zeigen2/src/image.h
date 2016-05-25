#ifndef Z_IMAGE_H
#define Z_IMAGE_H

#include <SDL2/SDL.h>
#include <cairo.h>
#include "types.h"
#include "environment.h"

struct Image_s {
    Unt refcount;
    float score;
    Value path; /* A Value so we can allow different sources than files */
    Unt size; /* approximate */
    Unt created;
    SDL_Surface *base;
    cairo_surface_t *surface;
};


cairo_surface_t *image_get_surface_from_file(Environment *environment, Value filename);
Bool resource_create_image(Environment *environment, Value skeleton, Unt initial_score, Unt *size);

#endif
