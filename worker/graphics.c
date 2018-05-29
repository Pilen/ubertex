#include <SDL2/SDL.h>
#include <cairo.h>
#include <pango/pangocairo.h>

#include "headers.h"

Color graphics_parse_color(Value raw) {
    /* Should work on a cons cell */
    (void) raw;
    log_fatal("Not yet implemented");
}

void graphics_clear(Environment *environment) {
    if (environment -> fast_run) {return;}
    cairo_set_source_rgba(environment -> cairo,
                          environment -> clear_red,
                          environment -> clear_green,
                          environment -> clear_blue,
                          environment -> clear_alpha);
    cairo_paint(environment -> cairo);
}

void graphics_present(Environment *environment) {
    if (environment -> fast_run) {return;}
    profiler_start(profile_present);
    void *pixels;
    int pitch;
    SDL_Rect rect;
    rect.x = 0;
    rect.y = 0;
    rect.w = environment -> width;
    rect.h = environment -> height;

    cairo_surface_flush(environment -> cairo_surface);
    SDL_UnlockTexture(environment -> base_texture);
    SDL_RenderCopy(environment -> renderer, environment -> base_texture, &rect, &rect);
    SDL_RenderPresent(environment -> renderer);
    SDL_LockTexture(environment -> base_texture, NULL, &pixels, &pitch);
    profiler_end(profile_present);
}

void graphics_render_at(Renderable *renderable, Double x, Double y, Environment *environment) {
    if (environment -> fast_run) {return;}
    cairo_save(environment -> cairo);
    cairo_translate(environment -> cairo, x, y);
    renderable -> render(renderable -> data, environment);
    cairo_restore(environment -> cairo);
}

void graphics_render_centered_at(Renderable *renderable, Double x, Double y, Environment *environment) {
    if (environment -> fast_run) {return;}
    cairo_save(environment -> cairo);
    Double w = renderable -> width;
    Double h = renderable -> height;
    cairo_translate(environment -> cairo, x - w/2, y - h/2);
    renderable -> render(renderable -> data, environment);
    cairo_restore(environment -> cairo);
}

/**
 * position must be a list
 * If the list consists of two numbers (x y) the surface will be rendered at location x,y
 * the same is true for (plain x y)
 * (x y)
 * (plain x y) x,y = numbers, rendered with top left corner at x,y with scale=1
 * (full) fullscreen
 * (centered)
 * (scaled)
 * (sized)
 * (rotated)

 * (windowed)
 */
Bool graphics_render_at_position(Renderable *renderable, Value position, Environment *environment) {
    if (environment -> fast_run) {return false;}
    profiler_start(profile_render);

    Double width = renderable -> width;
    Double height = renderable -> height;
    Double screen_width = environment -> width;
    Double screen_height = environment -> height;

    cairo_save(environment -> cairo);

    if (position.type == NIL) {
        cairo_scale(environment -> cairo, screen_width/width, screen_height/height);
        goto RENDER;
    }
    if (!IS_LIST(position)) {
        log_error_in;
        goto ERROR;
    }
    if (position.type != CONS) {
        log_error_in;
        goto ERROR;
    }
    Value length_val = list_length(position);
    if (length_val.type != INTEGER) {
        log_error_in;
        goto ERROR;
    }
    Unt length = NUM_VAL(length_val);
    Value first = NEXT(position);


    if (first.type == SYMBOL) {
        if (equal(first, symbols_plain)) {
            /**** plain ****/
            /* Render at coords, unscaled */
            if (length == 3) {
                Value x = NEXT(position);
                Value y = NEXT(position);
                if (IS_NUMERIC(x) && IS_NUMERIC(y)) {
                    cairo_translate(environment -> cairo, NUM_VAL(x), NUM_VAL(y));
                    goto RENDER;
                } /* Return a specific error? */
            }
        } else if (equal(first, symbols_full)) {
            /**** full ****/
            /* Stretch image to fill entire screen */
            if (length == 1) {
                cairo_scale(environment -> cairo, screen_width/width, screen_height/height);
                goto RENDER;
            }
        } else if (equal(first, symbols_centered)) {
            /**** centered ****/
            if (length == 3) {
                Value x = NEXT(position);
                Value y = NEXT(position);
                if (x.type == INTEGER && y.type == INTEGER) {
                    /* Render offset from center */
                    Double dx = (screen_width - width) / 2 + NUM_VAL(x);
                    Double dy = (screen_height - height) / 2 + NUM_VAL(y);
                    cairo_translate(environment -> cairo, dx, dy);
                    goto RENDER;
                }
                if (x.type == FLOAT && y.type == FLOAT) {
                    /* As in sized */
                    Double dx = (screen_width - width) / 2 + ((screen_width - width)/2 * NUM_VAL(x));
                    Double dy = (screen_height - height) / 2 + ((screen_height - height)/2 * NUM_VAL(y));
                    cairo_translate(environment -> cairo, dx, dy);
                    goto RENDER;
                }
            } else if (length == 1) {
                Double dx = (screen_width - width) / 2;
                Double dy = (screen_height - height) / 2;
                cairo_translate(environment -> cairo, dx, dy);
            }
        } else if (equal(first, symbols_scaled)) {
            /**** scaled ****/
            /* ('scaled x y scale) */
            /* ('scaled x y scalex scaley) */
            /* TODO: make it work with relative float positions */
            if (length < 4 || length > 5) {
                log_error_in;
                goto ERROR;
            }

            Value x_val = NEXT(position);
            Value y_val = NEXT(position);
            Value scale_x = NEXT(position);
            Value scale_y = scale_x;
            if (length == 5) {
                scale_y = NEXT(position);
            }

            Double new_width;
            Double new_height;
            if (scale_x.type == INTEGER) {
                new_width = NUM_VAL(scale_x);
            } else if (scale_x.type == FLOAT) {
                new_width = width * NUM_VAL(scale_x);
            } else {
                log_error_in;
                goto ERROR;
            }
            if (scale_y.type == INTEGER) {
                new_height = NUM_VAL(scale_y);
            } else if (scale_y.type == FLOAT) {
                new_height = height * NUM_VAL(scale_y);
            } else {
                log_error_in;
                goto ERROR;
            }

            Double x = 0;
            Double y = 0;
            if (x_val.type == INTEGER) {
                x = NUM_VAL(x_val);
            } else if (x_val.type == FLOAT) {
                x = (screen_width - new_width) / 2 + ((screen_width - new_width)/2 * NUM_VAL(x_val));
            } else {
                log_error_in;
                goto ERROR;
            }
            if (y_val.type == INTEGER) {
                y = NUM_VAL(y_val);
            } else if (y_val.type == FLOAT) {
                y = (screen_height - new_height) / 2 + ((screen_height - new_height)/2 * NUM_VAL(y_val));
            } else {
                log_error_in;
                goto ERROR;
            }
            cairo_translate(environment -> cairo, x, y);
            cairo_scale(environment -> cairo, new_width/width, new_height/height);
            goto RENDER;

        } else if (equal(first, symbols_sized)) {
            /**** sized ****/
            /* ('sized x y sizex/boundx sizey/boundy) */
            /* Render scaled but keep aspect ratio */
            if (length < 4 || length > 5) {
                log_error_in;
                goto ERROR;
            }
            Value x_val = NEXT(position);
            Value y_val = NEXT(position);
            Value size_x = NEXT(position);
            Value size_y = size_x;
            if (length == 5) {
                size_y = NEXT(position);
            }

            Double desired_width;
            Double desired_height;
            if (size_x.type == INTEGER) {
                desired_width = NUM_VAL(size_x);
            } else if (size_x.type == FLOAT) {
                desired_width = screen_width * NUM_VAL(size_x);
            } else {
                log_error_in;
                goto ERROR;
            }
            if (size_y.type == INTEGER) {
                desired_height = NUM_VAL(size_y);
            } else if (size_y.type == FLOAT) {
                desired_height = screen_height * NUM_VAL(size_y);
            } else {
                log_error_in;
                goto ERROR;
            }

            Double new_width;
            Double new_height;
            Double ratio_w = desired_width / width;
            Double ratio_h = desired_height / height;
            if (ratio_w <= ratio_h) {
                new_height = desired_width * ((Double) height / (Double) width);
                new_width = desired_width;
            } else {
                new_width = desired_height * ((Double) width / (Double) height);
                new_height = desired_height;
            }

            Double x = 0;
            Double y = 0;
            if (x_val.type == INTEGER) {
                x = NUM_VAL(x_val);
            } else if (x_val.type == FLOAT) {
                x = (screen_width - new_width) / 2 + ((screen_width - new_width)/2 * NUM_VAL(x_val));
            } else {
                log_error_in;
                goto ERROR;
            }
            if (y_val.type == INTEGER) {
                y = NUM_VAL(y_val);
            } else if (y_val.type == FLOAT) {
                y = (screen_height - new_height) / 2 + ((screen_height - new_height)/2 * NUM_VAL(y_val));
            } else {
                log_error_in;
                goto ERROR;
            }
            cairo_translate(environment -> cairo, x, y);
            cairo_scale(environment -> cairo, new_width/width, new_height/height);
            goto RENDER;
        } else if (equal(first, symbols_rotated)) {
            /**** Rotated ****/
            if (length < 4) {
                log_error_in;
                goto ERROR;
            }

            Value angle_v = NEXT(position);
            if (!IS_NUMERIC(angle_v)) {
                log_error_in;
                goto ERROR;
            }
            Double angle = NUM_VAL(angle_v);

            Value x = NEXT(position);
            Value y = NEXT(position);
            if (!IS_NUMERIC(x) || !IS_NUMERIC(y)) {
                log_error_in;
                goto ERROR;
            }
            Double dx = NUM_VAL(x);
            Double dy = NUM_VAL(y);

            Value scale_x;
            Value scale_y;
            if (length == 5) {
                scale_x = NEXT(position);
                scale_y = scale_x;
            } else if (length == 6) {
                scale_x = NEXT(position);
                scale_y = NEXT(position);
            } else {
                log_error_in;
                goto ERROR;
            }

            Double sx;
            Double sy;
            if (scale_x.type == INTEGER && scale_y.type == INTEGER) {
                sx = NUM_VAL(scale_x)/width;
                sy = NUM_VAL(scale_y)/height;
            } else if (scale_x.type == FLOAT && scale_y.type == FLOAT) {
                sx = NUM_VAL(scale_x);
                sy = NUM_VAL(scale_y);
            } else {
                log_error_in;
                goto ERROR;
            }
            if (sx == 0.0 || sy == 0.0) {
                /* Scaled to 0, thus is not shown. Cairo freezes if given scales of 0 */
                return true;
            }
            cairo_translate(environment -> cairo, dx, dy);
            cairo_translate(environment -> cairo, sx*width/2, sx*height/2);
            cairo_rotate(environment -> cairo, angle);
            cairo_scale(environment -> cairo, sx, sy);
            cairo_translate(environment -> cairo, -width/2, -height/2);
            goto RENDER;
        }

    } else if (IS_NUMERIC(first) && length == 2) {/* (x y) */
        Value second = NEXT(position);
        if (IS_NUMERIC(second)) {
            Double x = NUM_VAL(first);
            Double y = NUM_VAL(second);
            cairo_translate(environment -> cairo, x, y);
            goto RENDER;
        }
    }
 ERROR:
    cairo_restore(environment -> cairo);
    profiler_end(profile_render);
    return false;
 RENDER:
    renderable -> render(renderable -> data, environment);
    /* cairo_set_source_surface(environment -> cairo, surface, 0, 0); */
    /* cairo_paint(environment -> cairo); */
    cairo_restore(environment -> cairo);
    profiler_end(profile_render);
    return true;
}


void graphics_show_cairo_surface(void *data, Environment *environment) {
    if (environment -> fast_run) {return;}
    cairo_surface_t *surface = (cairo_surface_t *) data;
    cairo_set_source_surface(environment -> cairo, surface, 0, 0);
    profiler_start(profile_cairo);
    cairo_paint(environment -> cairo);
    profiler_end(profile_cairo);
}

void graphics_fill(Color *color, Environment *environment) {
    if (environment -> fast_run) {return;}
    cairo_set_source_rgba(environment -> cairo, color -> r, color -> g, color -> b, color -> a);
    cairo_paint(environment -> cairo);
}

void graphics_calibrate(Environment *environment) {
    if (environment -> fast_run) {return;}
    cairo_set_source_rgb(environment -> cairo, 255, 0, 255);
    cairo_paint(environment -> cairo);

    cairo_set_line_width(environment -> cairo, 1);

    Double x = 312;
    Double y = 250;
    Double width = 400;
    Double height = 220;
    cairo_rectangle(environment -> cairo,
                    x, y, width, height);
    cairo_set_source_rgb(environment -> cairo, 255, 255, 255);
    cairo_fill_preserve(environment -> cairo);
    cairo_set_source_rgb(environment -> cairo, 0, 0, 0);
    cairo_stroke(environment -> cairo);

    cairo_move_to(environment -> cairo, 0, 0);
    cairo_line_to(environment -> cairo, environment -> width, environment -> height);
    cairo_move_to(environment -> cairo, 0, environment -> height);
    cairo_line_to(environment -> cairo, environment -> width, 0);
    cairo_stroke(environment -> cairo);
}
