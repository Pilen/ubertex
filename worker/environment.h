#ifndef W_ENVIRONMENT_H
#define W_ENVIRONMENT_H

#include <SDL2/SDL.h>
#include <cairo.h>

typedef struct {
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Texture *base_texture;
    cairo_surface_t *cairo_surface;
    cairo_t *cairo;

    Unt frame;

    Bool fast_run;

    Int width;
    Int height;

    Hash *component_definitions;
    Component *current_component;
    Int current_layer;
    Layer *layers_foreground;
    Layer *layers_background;

    Value messages;
    Value last_message;

    Value update; /* expression */
    Value background; /* expression */
    Value foreground; /* expression */

    Double clear_red;
    Double clear_green;
    Double clear_blue;
    Double clear_alpha;

    Value dynamic_variables;
    Value lexical_variables;
    Hash *global_variables;
    Hash *functions;

    Value call_stack; /* list */
} Environment;

Environment *environment_create(void);
void environment_bind_variables(Value bindings, Environment *environment);
void environment_unbind_variables(Environment *environment);
void environment_bind_lexical(Value bindings, Environment *environment);
void environment_unbind_lexical(Environment *environment);
Unt environment_bind_multiple_variables(Value bindings, Environment *environment);
void environment_unbind_multiple_variables(Unt count, Environment *environment);
Bool environment_lookup_variable(Value key, Value *result, Environment *environment);
void environment_set_variable(Value key, Value value, Environment *environment);

#endif
