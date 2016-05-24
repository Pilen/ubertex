#ifndef Z_ENVIRONMENT_H
#define Z_ENVIRONMENT_H

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

    Double setting_clear_red;
    Double setting_clear_green;
    Double setting_clear_blue;
    Double setting_clear_alpha;

    Hash *variables;
    Hash *functions;

    List *call_stack;
} Environment;

Environment *environment_create(void);

#endif
