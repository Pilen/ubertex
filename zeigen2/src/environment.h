#ifndef Z_ENVIRONMENT_H
#define Z_ENVIRONMENT_H

#include <SDL2/SDL.h>
#include "hash.h"

typedef struct {
    SDL_Window *window;
    SDL_Renderer *renderer;

    Value component_next_update;
    List *component_next_update_args;

    Bool setting_clear;
    char setting_clear_red;
    char setting_clear_green;
    char setting_clear_blue;
    char setting_clear_alpha;
    Hash *other_settings;
    Int setting_screen_width; /* ??? */
    Int setting_screen_height; /* ??? */

    Hash *variables;
    Hash *functions;
} Environment;

Environment *environment_create(void);

#endif
