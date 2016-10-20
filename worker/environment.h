#ifndef W_ENVIRONMENT_H
#define W_ENVIRONMENT_H

#include <SDL2/SDL.h>
#include <cairo.h>
#include "hash.h"
#include "layer.h"

typedef struct {
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Texture *base_texture;
    cairo_surface_t *cairo_surface;
    cairo_t *cairo;

    Unt skip_ticks; /* 1000 / FPS */
    Bool fast_run;

    Int width;
    Int height;

    Hash *component_definitions;
    Component *current_component;
    Int current_layer;
    Layer *layers;

    Value messages;
    Value last_message;

    Value update; /* expression */
    Value render; /* expression */

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
Bool environment_lookup_variable(Value key, Value *result, Environment *environment);
void environment_set_variable(Value key, Value value, Environment *environment);

#endif
