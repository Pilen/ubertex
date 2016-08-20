#ifndef W_IMAGE_H
#define W_IMAGE_H

#include <SDL2/SDL.h>
#include <cairo.h>
#include "types.h"
#include "environment.h"

struct Image_s {
    Unt refcount;
    Unt created;
    Unt last_use;
    Unt size; /* approximate */

    Value path; /* A Value so we can allow different sources than files */
    SDL_Surface *base;
    cairo_surface_t *surface;
};

Bool image_get_renderable_from_file(Environment *environment, Value filename, Renderable *target);
Unt resource_create_image(Environment *environment, Value skeleton);

#endif
