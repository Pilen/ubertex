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

Bool graphics_render_at_position(Environment *environment, cairo_surface_t *surface, Value position) {
    return false;
 /*    /\* TODO: really needs to be cleaned up!!! *\/ */
 /*    SDL_RendererFlip flip = SDL_FLIP_NONE; */
 /*    Float angle = 0; */
 /*    /\* SDL_Point pivot_actual; *\/ */
 /*    SDL_Point *pivot = NULL; */

 /*    SDL_Rect image; */
 /*    image.x = 0; */
 /*    image.y = 0; */
 /*    SDL_QueryTexture(texture, NULL, NULL, &image.w, &image.h); */

 /*    Int window_w; */
 /*    Int window_h; */
 /*    SDL_GetWindowSize(environment -> window, &window_w, &window_h); */

 /*    if (position.type != LIST) { */
 /*        log_error_in; */
 /*        return false; */
 /*    } */
 /*    List *list = position.val.list_val; */
 /*    if (list -> length <= 0) { */
 /*        log_error_in; */
 /*        return false; */
 /*    } */
 /*    Value first = LIST_GET_UNSAFE(list, 0); */
 /*    if (first.type == SYMBOL) { */
 /*        if (equal(first, symbols_plain)) { */
 /*            /\* plain *\/ */
 /*            /\* Render at coords, unscaled *\/ */
 /*            if (list -> length == 3) { */
 /*                Value x = LIST_GET_UNSAFE(list, 1); */
 /*                Value y = LIST_GET_UNSAFE(list, 2); */
 /*                if (x.type == INTEGER && y.type == INTEGER) { */
 /*                    image.x = x.val.integer_val; */
 /*                    image.y = y.val.integer_val; */
 /*                    goto RENDER; */
 /*                } else if (x.type == FLOAT && y.type == FLOAT) { */
 /*                    image.x = (Int) x.val.float_val; */
 /*                    image.y = (Int) y.val.float_val; */
 /*                    goto RENDER; */
 /*                } */
 /*            } */
 /*        } else if (equal(first, symbols_full)) { */
 /*            /\* full *\/ */
 /*            /\* Stretch image to fill entire screen *\/ */
 /*            if (list -> length == 1) { */
 /*                image.w = window_w; */
 /*                image.h = window_h; */
 /*                goto RENDER; */
 /*            } */
 /*        } else if (equal(first, symbols_centered)) { */
 /*            /\* centered *\/ */
 /*            if (list -> length == 3) { */
 /*                /\* Render offset from center *\/ */
 /*                Value x = LIST_GET_UNSAFE(list, 1); */
 /*                Value y = LIST_GET_UNSAFE(list, 2); */
 /*                if (x.type == INTEGER && y.type == INTEGER) { */
 /*                    image.x = (window_w - image.w) / 2 + x.val.integer_val; */
 /*                    image.y = (window_h - image.h) / 2 + y.val.integer_val; */
 /*                    goto RENDER; */
 /*                } */
 /*                if (x.type == FLOAT && y.type == FLOAT) { */
 /*                    /\* As in sized *\/ */
 /*                    image.x = (window_w - image.w) / 2 + ((window_w - image.w)/2 * x.val.float_val); */
 /*                    image.y = (window_h - image.h) / 2 + ((window_h - image.h)/2 * y.val.float_val); */
 /*                    goto RENDER; */
 /*                } */

 /*            } else if (list -> length == 1) { */
 /*                /\* Render at center *\/ */
 /*                image.x = (window_w - image.w) / 2; */
 /*                image.y = (window_h - image.h) / 2; */
 /*                goto RENDER; */
 /*            } */
 /*        } else if (equal(first, symbols_scaled)) { */
 /*            /\* scaled *\/ */
 /*            if (list -> length < 3) { */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            Value x = LIST_GET_UNSAFE(list, 1); */
 /*            Value y = LIST_GET_UNSAFE(list, 2); */
 /*            switch (x.type) { */
 /*            case INTEGER: */
 /*                image.x = x.val.integer_val; */
 /*                break; */
 /*            case FLOAT: */
 /*                image.x = (Int) x.val.float_val; */
 /*                break; */
 /*            default: */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            switch (y.type) { */
 /*            case INTEGER: */
 /*                image.y = y.val.integer_val; */
 /*                break; */
 /*            case FLOAT: */
 /*                image.y = (Int) y.val.float_val; */
 /*                break; */
 /*            default: */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            Value x_scale; */
 /*            Value y_scale; */
 /*            if (list -> length == 4) { */
 /*                x_scale = LIST_GET_UNSAFE(list, 3); */
 /*                y_scale = x_scale; */
 /*            } else if (list -> length == 5) { */
 /*                x_scale = LIST_GET_UNSAFE(list, 3); */
 /*                y_scale = LIST_GET_UNSAFE(list, 4); */
 /*            } else { */
 /*                log_error_in; */
 /*                return false; */
 /*            } */

 /*            if (x_scale.type == INTEGER && y_scale.type == INTEGER) { */
 /*                image.w = x_scale.val.integer_val; */
 /*                image.h = y_scale.val.integer_val; */
 /*            } else if (x_scale.type == FLOAT && y_scale.type == FLOAT) { */
 /*                image.w *= x_scale.val.float_val; */
 /*                image.h *= y_scale.val.float_val; */
 /*            } else { */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            goto RENDER; */
 /*        } else if (equal(first, symbols_sized)) { */
 /*            /\* sized *\/ */
 /*            /\* Render scaled but keep aspect ratio *\/ */
 /*            if (list -> length == 5) { */
 /*                Value offset_xv = LIST_GET_UNSAFE(list, 1); */
 /*                Value offset_yv = LIST_GET_UNSAFE(list, 2); */
 /*                Value size_xv = LIST_GET_UNSAFE(list, 3); */
 /*                Value size_yv = LIST_GET_UNSAFE(list, 4); */

 /*                Float width; */
 /*                Float height; */
 /*                if (size_xv.type == INTEGER && size_yv.type == INTEGER) { */
 /*                    width = size_xv.val.integer_val; */
 /*                    height = size_yv.val.integer_val; */
 /*                } else if (size_xv.type == FLOAT && size_yv.type == FLOAT) { */
 /*                    width = window_w * size_xv.val.float_val; */
 /*                    height = window_h * size_yv.val.float_val; */
 /*                } else { */
 /*                    log_error_in; */
 /*                    return false; */
 /*                } */
 /*                Float ratio_w = width / image.w; */
 /*                Float ratio_h = height / image.h; */
 /*                if (ratio_w <= ratio_h) { */
 /*                    image.h = width * ((Float) image.h / (Float) image.w); */
 /*                    image.w = width; */
 /*                    /\* image.y = (window_h - image.h) / 2; *\/ */
 /*                } else { */
 /*                    image.w = height * ((Float) image.w / (Float) image.h); */
 /*                    image.h = height; */
 /*                    /\* image.x = (window_w - image.w) / 2; *\/ */
 /*                } */

 /*                if (offset_xv.type == INTEGER && offset_yv.type == INTEGER) { */
 /*                    /\* debugi((window_w - image.w) / 2); *\/ */
 /*                    /\* debugi((window_h - image.h) / 2); *\/ */
 /*                    /\* image.x = (window_w - image.w) / 2 + offset_xv.val.integer_val; *\/ */
 /*                    /\* image.y = (window_h - image.h) / 2 + offset_yv.val.integer_val; *\/ */
 /*                    image.x += offset_xv.val.integer_val; */
 /*                    image.y += offset_yv.val.integer_val; */
 /*                } else if (offset_xv.type == FLOAT && offset_yv.type == FLOAT) { */
 /*                    /\* image.x = window_w * offset_xv.val.float_val; *\/ */
 /*                    /\* image.y = window_h * offset_yv.val.float_val; *\/ */
 /*                    /\* image.x = (window_w - image.w) / 2 + (window_w * offset_xv.val.float_val); *\/ */
 /*                    /\* image.y = (window_h - image.h) / 2 + (window_h * offset_yv.val.float_val); *\/ */
 /*                    image.x = (window_w - image.w) / 2 + ((window_w - image.w)/2 * offset_xv.val.float_val); */
 /*                    image.y = (window_h - image.h) / 2 + ((window_h - image.h)/2 * offset_yv.val.float_val); */
 /*                    /\* image.x = (window_w - image.w) * offset_xv.val.float_val; *\/ */
 /*                    /\* image.y = (window_h - image.h) * offset_yv.val.float_val; *\/ */


 /*                    /\* image.x = (width - image.w) / 2 + offset_xv.val.float_val; *\/ */
 /*                    /\* image.y = (height - image.h) / 2 + offset_yv.val.float_val; *\/ */
 /*                } else { */
 /*                    log_error_in; */
 /*                    return false; */
 /*                } */
 /*                goto RENDER; */
 /*            } */
 /*        } else if (equal(first, symbols_rotated)) { */
 /*            if (list -> length < 4) { */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            Value angle_v = LIST_GET_UNSAFE(list, 1); */
 /*            switch (angle_v.type) { */
 /*            case INTEGER: */
 /*                angle = (Float) angle_v.val.integer_val; */
 /*                break; */
 /*            case FLOAT: */
 /*                angle = angle_v.val.float_val; */
 /*                break; */
 /*            default: */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            Value x = LIST_GET_UNSAFE(list, 2); */
 /*            Value y = LIST_GET_UNSAFE(list, 3); */
 /*            switch (x.type) { */
 /*            case INTEGER: */
 /*                image.x = x.val.integer_val; */
 /*                break; */
 /*            case FLOAT: */
 /*                image.x = (Int) x.val.float_val; */
 /*                break; */
 /*            default: */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            switch (y.type) { */
 /*            case INTEGER: */
 /*                image.y = y.val.integer_val; */
 /*                break; */
 /*            case FLOAT: */
 /*                image.y = (Int) y.val.float_val; */
 /*                break; */
 /*            default: */
 /*                log_error_in; */
 /*                return false; */
 /*            } */
 /*            Value x_scale; */
 /*            Value y_scale; */
 /*            if (list -> length == 5) { */
 /*                x_scale = LIST_GET_UNSAFE(list, 4); */
 /*                y_scale = x_scale; */
 /*            } else if (list -> length == 6) { */
 /*                x_scale = LIST_GET_UNSAFE(list, 4); */
 /*                y_scale = LIST_GET_UNSAFE(list, 5); */
 /*            } else { */
 /*                log_error_in; */
 /*                return false; */
 /*            } */

 /*            if (x_scale.type == INTEGER && y_scale.type == INTEGER) { */
 /*                image.w = x_scale.val.integer_val; */
 /*                image.h = y_scale.val.integer_val; */
 /*            } else if (x_scale.type == FLOAT && y_scale.type == FLOAT) { */
 /*                image.w *= x_scale.val.float_val; */
 /*                image.h *= y_scale.val.float_val; */
 /*            } else { */
 /*                log_error_in; */
 /*                return false; */
 /*            } */

 /*            pivot = NULL; */

 /*            goto RENDER_EX; */





 /*        } */
 /*    } else if (first.type == INTEGER && list -> length == 2) { */
 /*        Value second = LIST_GET_UNSAFE(list, 1); */
 /*        if (second.type == INTEGER) { */
 /*            image.x = first.val.integer_val; */
 /*            image.y = second.val.integer_val; */
 /*            goto RENDER; */
 /*        } */
 /*    } */
 /*    log_error_in; */
 /*    return false; */

 /* RENDER: */
 /*    SDL_RenderCopy(environment -> renderer, texture, NULL, &image); */
 /*    return true; */
 /* RENDER_EX: */
 /*    SDL_RenderCopyEx(environment -> renderer, texture, NULL, &image, angle, pivot, flip); */
 /*    return true; */
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
