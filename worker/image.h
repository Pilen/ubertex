#ifndef W_IMAGE_H
#define W_IMAGE_H

#include <SDL2/SDL.h>
#include <cairo.h>

struct Image_s {
    Unt refcount;
    Unt created;
    Unt last_use;
    Unt size; /* approximate */

    Value path; /* A Value so we can allow different sources than files */
    SDL_Surface *base;
    cairo_surface_t *surface;
};

Bool image_get_renderable_from_file(Value filename, Renderable *target, Environment *environment);
Unt resource_create_image(Value skeleton, Environment *environment);

#endif
