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

    Value component_next_update;
    List *component_next_update_args;

    Value component_next_post;
    List *component_next_post_args;

    /* TODO: decide color type */
    uint8_t setting_clear_red;
    uint8_t setting_clear_green;
    uint8_t setting_clear_blue;
    uint8_t setting_clear_alpha;

    Hash *other_settings;
    Int setting_screen_width; /* ??? */
    Int setting_screen_height; /* ??? */

    Hash *variables;
    Hash *functions;

    List *call_stack;
} Environment;

Environment *environment_create(void);

#endif
