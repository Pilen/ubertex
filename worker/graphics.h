#ifndef W_GRAPHICS_H
#define W_GRAPHICS_H

#include "types.h"
#include "environment.h"

struct Renderable_s {
    void *data;
    void (*render)(void *data, Environment *environment);
    Double width;
    Double height;
};

struct Color_s {
    Double r;
    Double g;
    Double b;
    Double a;
};

void graphics_clear(Environment *environment);
void graphics_present(Environment *environment);
void graphics_render_at(Renderable *renderable, Double x, Double y, Environment *environment);
void graphics_render_centered_at(Renderable *renderable, Double x, Double y, Environment *environment);
Bool graphics_render_at_position(Renderable *renderable, Value position, Environment *environment);
void graphics_show_cairo_surface(void *data, Environment *environment);
void graphics_fill(Double red, Double green, Double blue, Double alpha, Environment *environment);
void graphics_calibrate(Environment *environment);

#endif
