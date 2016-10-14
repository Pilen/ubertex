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

    Value dynamic_variables;
    Hash *global_variables;
    Hash *functions;

    Value call_stack; /* list */
} Environment;

Environment *environment_create(void);
void environment_bind_variables(Value bindings, Environment *environment);
void environment_unbind_variables(Environment *environment);
Bool environment_lookup_variable(Environment *environment, Value key, Value *result);
void environment_set_variable(Environment *environment, Value key, Value value);

#endif
