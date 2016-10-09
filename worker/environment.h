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
    Value component_next_update_args; /* list */

    Value component_next_post;
    Value component_next_post_args; /* list */

    Double clear_red;
    Double clear_green;
    Double clear_blue;
    Double clear_alpha;

    Hash *variables;
    Hash *functions;

    Value call_stack; /* list */
} Environment;

Environment *environment_create(void);

#endif
