#include <SDL2/SDL.h>
#include <cairo.h>
#include <pango/pangocairo.h>
#include "graphics.h"
#include "list.h"
#include "symbol.h"
#include "debug.h"
#include "basic.h"

void graphics_clear(Environment *environment) {
    cairo_set_source_rgba(environment -> cairo,
                          environment -> clear_red,
                          environment -> clear_green,
                          environment -> clear_blue,
                          environment -> clear_alpha);
    cairo_paint(environment -> cairo);
}
void graphics_present(Environment *environment) {
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

}
void graphics_render_at(Environment *environment, cairo_surface_t *surface, Double x, Double y) {
    cairo_set_source_surface(environment -> cairo, surface, x, y);
    cairo_paint(environment -> cairo);
}

void graphics_render_centered_at(Environment *environment, cairo_surface_t *surface, Double x, Double y) {
    Int width = cairo_image_surface_get_width(surface);
    Int height = cairo_image_surface_get_height(surface);
    cairo_set_source_surface(environment -> cairo, surface, x - width/2, y - height/2);
    cairo_paint(environment -> cairo);
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
Bool graphics_render_at_position(Environment *environment, cairo_surface_t *surface, Value position) {
    Double x;
    Double y;

    Int img_width = cairo_image_surface_get_width(surface);
    Int img_height = cairo_image_surface_get_height(surface);
    Int screen_width = environment -> width;
    Int screen_height = environment -> height;

    if (position.type != LIST) {
        log_error_in;
        return false;
    }
    List *list = position.val.list_val;
    if (list -> length <= 0) {
        log_error_in;
        return false;
    }
    Value first = LIST_GET_UNSAFE(list, 0);

    cairo_save(environment -> cairo);

    if (first.type == SYMBOL) {
        if (equal(first, symbols_plain)) {
            /**** plain ****/
            /* Render at coords, unscaled */
            if (list -> length == 3) {
                Value x = LIST_GET_UNSAFE(list, 1);
                Value y = LIST_GET_UNSAFE(list, 2);
                if (IS_NUMERIC(x) && IS_NUMERIC(y)) {
                    cairo_translate(environment -> cairo, NUM_VAL(x), NUM_VAL(y));
                    goto RENDER;
                } /* Return a specific error? */
            }
        } else if (equal(first, symbols_full)) {
            /**** full ****/
            /* Stretch image to fill entire screen */
            if (list -> length == 1) {
                cairo_scale(environment -> cairo, screen_width/img_width, screen_height/img_height);
                goto RENDER;
            }
        } else if (equal(first, symbols_centered)) {
            /**** centered ****/
            if (list -> length == 3) {
                Value x = LIST_GET_UNSAFE(list, 1);
                Value y = LIST_GET_UNSAFE(list, 2);
                if (x.type == INTEGER && y.type == INTEGER) {
                    /* Render offset from center */
                    Double dx = (screen_width - img_width) / 2 + NUM_VAL(x);
                    Double dy = (screen_height - img_height) / 2 + NUM_VAL(y);
                    cairo_translate(environment -> cairo, dx, dy);
                    goto RENDER;
                }
                if (x.type == FLOAT && y.type == FLOAT) {
                    /* As in sized */
                    Double dx = (screen_width - img_width) / 2 + ((screen_width - img_width)/2 * NUM_VAL(x));
                    Double dy = (screen_height - img_height) / 2 + ((screen_height - img_height)/2 * NUM_VAL(y));
                    cairo_translate(environment -> cairo, dx, dy);
                    goto RENDER;
                }
            } else if (list -> length == 1) {
                Double dx = (screen_width - img_width) / 2;
                Double dy = (screen_height - img_height) / 2;
                cairo_translate(environment -> cairo, dx, dy);
            }
        } else if (equal(first, symbols_scaled)) {
            /**** scaled ****/
            /* TODO: make it work with relative float positions */
            if (list -> length < 3) {
                log_error_in;
                goto ERROR;
            }
            Value a = LIST_GET_UNSAFE(list, 1);
            Value b = LIST_GET_UNSAFE(list, 2);
            if (!IS_NUMERIC(a) || !IS_NUMERIC(b)) {
                log_error_in;
                goto ERROR;
            }
            Double x = NUM_VAL(a);
            Double y = NUM_VAL(b);

            Value x_scale;
            Value y_scale;
            if (list -> length == 4) {
                x_scale = LIST_GET_UNSAFE(list, 3);
                y_scale = x_scale;
            } else if (list -> length == 5) {
                x_scale = LIST_GET_UNSAFE(list, 3);
                y_scale = LIST_GET_UNSAFE(list, 4);
            } else {
                log_error_in;
                goto ERROR;
            }
            cairo_translate(environment -> cairo, x, y);
            if (x_scale.type == INTEGER && y_scale.type == INTEGER) {
                /* scale to absolute size (set size) */
                cairo_scale(environment -> cairo, NUM_VAL(x_scale)/img_width, NUM_VAL(y_scale)/img_height);
            } else if (x_scale.type == FLOAT && y_scale.type == FLOAT) {
                /* scale relative */
                cairo_scale(environment -> cairo, NUM_VAL(x_scale), NUM_VAL(y_scale));
            } else {
                log_error_in;
                goto ERROR;
            }
            goto RENDER;

        } else if (equal(first, symbols_sized)) {
            /* sized */
            /* Render scaled but keep aspect ratio */
            if (list -> length != 5) {
                log_error_in;
                goto ERROR;
            }
            Value offset_xv = LIST_GET_UNSAFE(list, 1);
            Value offset_yv = LIST_GET_UNSAFE(list, 2);
            Value size_xv = LIST_GET_UNSAFE(list, 3);
            Value size_yv = LIST_GET_UNSAFE(list, 4);

            Double width;
            Double height;
            if (size_xv.type == INTEGER && size_yv.type == INTEGER) {
                width = NUM_VAL(size_xv);
                height = NUM_VAL(size_yv);
            } else if (size_xv.type == FLOAT && size_yv.type == FLOAT) {
                width = screen_width * NUM_VAL(size_xv);
                height = screen_height * NUM_VAL(size_yv);
            } else {
                log_error_in;
                goto ERROR;
            }

            Double new_width;
            Double new_height;
            Double ratio_w = width / img_width;
            Double ratio_h = height / img_height;
            if (ratio_w <= ratio_h) {
                new_height = width * ((Double) img_height / (Double) img_width);
                new_width = width;
            } else {
                new_width = height * ((Double) img_width / (Double) img_height);
                new_height = height;
            }

            Double dx = 0;
            Double dy = 0;
            if (offset_xv.type == INTEGER && offset_yv.type == INTEGER) {
                dx = NUM_VAL(offset_xv);
                dy = NUM_VAL(offset_yv);
            } else if (offset_xv.type == FLOAT && offset_yv.type == FLOAT) {
                dx = (screen_width - new_width) / 2 + ((screen_width - new_width)/2 * NUM_VAL(offset_xv));
                dy = (screen_height - new_height) / 2 + ((screen_height - new_height)/2 * NUM_VAL(offset_yv));
            } else {
                log_error_in;
                goto ERROR;
            }
            cairo_translate(environment -> cairo, dx, dy);
            cairo_scale(environment -> cairo, new_width/img_width, new_height/img_height);
            goto RENDER;
        } else if (equal(first, symbols_rotated)) {
            /**** Rotated ****/
            if (list -> length < 4) {
                log_error_in;
                goto ERROR;
            }

            Value angle_v = LIST_GET_UNSAFE(list, 1);
            if (!IS_NUMERIC(angle_v)) {
                log_error_in;
                goto ERROR;
            }
            Double angle = NUM_VAL(angle_v);

            Value x = LIST_GET_UNSAFE(list, 2);
            Value y = LIST_GET_UNSAFE(list, 3);
            if (!IS_NUMERIC(x) || !IS_NUMERIC(y)) {
                log_error_in;
                goto ERROR;
            }
            Double dx = NUM_VAL(x);
            Double dy = NUM_VAL(y);

            Value x_scale;
            Value y_scale;
            if (list -> length == 5) {
                x_scale = LIST_GET_UNSAFE(list, 4);
                y_scale = x_scale;
            } else if (list -> length == 6) {
                x_scale = LIST_GET_UNSAFE(list, 4);
                y_scale = LIST_GET_UNSAFE(list, 5);
            } else {
                log_error_in;
                goto ERROR;
            }

            Double sx;
            Double sy;
            if (x_scale.type == INTEGER && y_scale.type == INTEGER) {
                sx = NUM_VAL(x_scale)/img_width;
                sy = NUM_VAL(y_scale)/img_height;
            } else if (x_scale.type == FLOAT && y_scale.type == FLOAT) {
                sx = NUM_VAL(x_scale);
                sy = NUM_VAL(y_scale);
            } else {
                log_error_in;
                goto ERROR;
            }
            cairo_translate(environment -> cairo, dx, dy);
            cairo_translate(environment -> cairo, sx*img_width/2, sx*img_height/2);
            cairo_rotate(environment -> cairo, angle);
            cairo_scale(environment -> cairo, sx, sy);
            cairo_translate(environment -> cairo, -img_width/2, -img_height/2);
            goto RENDER;
        }

    } else if (IS_NUMERIC(first) && list -> length == 2) {/* (x y) */
        Value second = LIST_GET_UNSAFE(list, 1);
        if (IS_NUMERIC(second)) {
            x = NUM_VAL(first);
            y = NUM_VAL(second);
            cairo_translate(environment -> cairo, x, y);
            goto RENDER;
        }
    }
 ERROR:
    cairo_restore(environment -> cairo);
    return false;

 RENDER:
    cairo_set_source_surface(environment -> cairo, surface, 0, 0);
    cairo_paint(environment -> cairo);
    cairo_restore(environment -> cairo);
    return true;
}


void graphics_fill(Environment *environment, Double red, Double green, Double blue, Double alpha) {
    cairo_set_source_rgba(environment -> cairo, red, green, blue, alpha);
    cairo_paint(environment -> cairo);
}

void graphics_calibrate(Environment *environment) {
    SDL_Rect rect;
    rect.x = 312;
    rect.y = 250;
    rect.w = 400;
    rect.h = 220;
    SDL_SetRenderDrawColor(environment -> renderer, 255, 0, 255, 255);
    SDL_RenderFillRect(environment -> renderer, NULL);

    SDL_SetRenderDrawColor(environment -> renderer, 255, 255, 255, 255);
    SDL_RenderFillRect(environment -> renderer, &rect);
    SDL_SetRenderDrawColor(environment -> renderer, 0, 0, 0, 255);
    SDL_RenderDrawRect(environment -> renderer, &rect);

    Int width;
    Int height;
    SDL_GetWindowSize(environment -> window, &width, &height);

    SDL_RenderDrawLine(environment -> renderer, 0, 0, width, height);
    SDL_RenderDrawLine(environment -> renderer, 0, height, width, 0);
}
