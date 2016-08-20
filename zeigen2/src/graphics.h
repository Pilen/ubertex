#ifndef W_GRAPHICS_H
#define W_GRAPHICS_H

#include "types.h"
#include "environment.h"

struct Renderable_s {
    void *data;
    void (*render)(Environment *environment, void *data);
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
void graphics_render_at(Environment *environment, Renderable *renderable, Double x, Double y);
void graphics_render_centered_at(Environment *environment, Renderable *renderable, Double x, Double y);
Bool graphics_render_at_position(Environment *environment, Renderable *renderable, Value position);
void graphics_show_cairo_surface(Environment *environment, void *data);
void graphics_fill(Environment *environment, Double red, Double green, Double blue, Double alpha);
void graphics_calibrate(Environment *environment);

#endif
