#ifndef Z_GRAPHICS_H
#define Z_GRAPHICS_H

#include "types.h"
#include "environment.h"

void graphics_clear(Environment *environment);
void graphics_present(Environment *environment);
void graphics_render_at(Environment *environment, cairo_surface_t *surface, Double x, Double y);
void graphics_render_centered_at(Environment *environment, cairo_surface_t *surface, Double x, Double y);
Bool graphics_render_at_position(Environment *environment, cairo_surface_t *surface, Value position);
void graphics_fill(Environment *environment, Double red, Double green, Double blue, Double alpha);
void graphics_calibrate(Environment *environment);

Bool graphics_move_to_position(Environment *environment, Double width, Double height, Value position);

#endif
