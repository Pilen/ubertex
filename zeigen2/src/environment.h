#ifndef W_ENVIRONMENT_H
#define W_ENVIRONMENT_H

#include <SDL2/SDL.h>
#include <cairo.h>
#include "hash.h"

typedef struct {
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Texture *base_texture;
    cairo_surface_t *cairo_surface;
    cairo_t *cairo;

    Int width;
    Int height;

    Value component_next_update;
    List *component_next_update_args;

    Value component_next_post;
    List *component_next_post_args;

    Double clear_red;
    Double clear_green;
    Double clear_blue;
    Double clear_alpha;

    Hash *variables;
    Hash *functions;

    List *call_stack;
} Environment;

Environment *environment_create(void);

#endif
