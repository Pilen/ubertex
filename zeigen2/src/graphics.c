#include <SDL2/SDL.h>
#include <cairo.h>
#include "graphics.h"
#include "list.h"
#include "symbol.h"
#include "debug.h"
#include "basic.h"

void graphics_render_at(Environment *environment, SDL_Texture *texture, Int x, Int y) {
    SDL_Rect dest;
    dest.x = x;
    dest.y = y;
    SDL_QueryTexture(texture, NULL, NULL, &dest.w, &dest.h);
    SDL_RenderCopy(environment -> renderer, texture, NULL, &dest);
}

Bool graphics_render_at_position(Environment *environment, SDL_Texture *texture, Value position) {
    SDL_Rect image;
    image.x = 0;
    image.y = 0;
    SDL_QueryTexture(texture, NULL, NULL, &image.w, &image.h);

    Int window_w;
    Int window_h;
    SDL_GetWindowSize(environment -> window, &window_w, &window_h);

    if (position.type != LIST) {
        return false;
    }
    List *list = position.val.list_val;
    if (list -> length <= 0) {
        return false;
    }
    Value first = LIST_GET_UNSAFE(list, 0);
    if (first.type == SYMBOL) {
        if (equal(first, symbols_plain)) {
            /* Render at coords, unscaled */
            if (list -> length == 3) {
                Value x = LIST_GET_UNSAFE(list, 1);
                Value y = LIST_GET_UNSAFE(list, 2);
                if (x.type == INTEGER && y.type == INTEGER) {
                    image.x = x.val.integer_val;
                    image.y = y.val.integer_val;
                    goto RENDER;
                }
            }
        } else if (equal(first, symbols_full)) {
            /* Stretch image to fill entire screen */
            if (list -> length == 1) {
                image.w = window_w;
                image.h = window_h;
                goto RENDER;
            }
        } else if (equal(first, symbols_centered)) {
            if (list -> length == 3) {
                /* Render offset from center */
                Value x = LIST_GET_UNSAFE(list, 1);
                Value y = LIST_GET_UNSAFE(list, 2);
                if (x.type == INTEGER && y.type == INTEGER) {
                    image.x = (window_w - image.w) / 2 + x.val.integer_val;
                    image.y = (window_h - image.h) / 2 + y.val.integer_val;
                    goto RENDER;
                }
            } else if (list -> length == 1) {
                /* Render at center */
                image.x = (window_w - image.w) / 2;
                image.y = (window_h - image.h) / 2;
                goto RENDER;
            }
        } else if (equal(first, symbols_sized)) {
            /* Render scaled but keep aspect ratio */
            if (list -> length == 5) {
                Value offset_xv = LIST_GET_UNSAFE(list, 1);
                Value offset_yv = LIST_GET_UNSAFE(list, 2);
                Value size_xv = LIST_GET_UNSAFE(list, 3);
                Value size_yv = LIST_GET_UNSAFE(list, 4);

                if (offset_xv.type == INTEGER && offset_yv.type == INTEGER) {
                    Int offset_x = offset_xv.val.integer_val;
                    Int offset_y = offset_yv.val.integer_val;

                    Float width;
                    Float height;
                    if (size_xv.type == INTEGER && size_yv.type == INTEGER) {
                        width = size_xv.val.integer_val;
                        height = size_yv.val.integer_val;
                    } else if (size_xv.type == FLOAT && size_yv.type == FLOAT) {
                        width = window_w * size_xv.val.float_val;
                        height = window_h * size_yv.val.float_val;
                    } else {
                        return false;
                    }
                    Float ratio_w = width / image.w;
                    Float ratio_h = height / image.h;
                    if (ratio_w <= ratio_h) {
                        image.h = width * ((Float) image.h / (Float) image.w);
                    } else {
                        image.w = height * ((Float) image.w / (Float) image.h);
                    }
                    image.x = (width - image.w) / 2 + offset_x;
                    image.y = (height - image.h) / 2 + offset_y;
                    goto RENDER;
                }
            }

        }
    } else if (first.type == INTEGER && list -> length == 2) {
        Value second = LIST_GET_UNSAFE(list, 1);
        if (second.type == INTEGER) {
            image.x = first.val.integer_val;
            image.y = second.val.integer_val;
            goto RENDER;
        }
    }
    return false;

 RENDER:
    SDL_RenderCopy(environment -> renderer, texture, NULL, &image);
    return true;
}

void graphics_cairo_test(Environment *environment) {
    Int width = 100;
    Int height = 100;
    SDL_Surface *sdl_surface = SDL_CreateRGBSurface(0, width, height, 32,
                                                    0x00FF0000,
                                                    0x0000FF00,
                                                    0x000000FF,
                                                    0);

    cairo_surface_t *cairo_surface = cairo_image_surface_create_for_data(sdl_surface -> pixels,
                                                                         CAIRO_FORMAT_RGB24,
                                                                         sdl_surface -> w,
                                                                         sdl_surface -> h,
                                                                         sdl_surface -> pitch);
    cairo_t *cairo = cairo_create(cairo_surface);
    cairo_select_font_face(cairo, "serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size(cairo, 32.0);
    cairo_set_source_rgb(cairo, 0.0, 1.0, 1.0);
    cairo_move_to(cairo, 10.0, 50.0);
    cairo_show_text(cairo, "Hello, World!");

    SDL_Texture *texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface);
    graphics_render_at(environment, texture, 10, 15);

    cairo_destroy(cairo);
    cairo_surface_write_to_png(cairo_surface, "/tmp/hello.png");
    cairo_surface_destroy(cairo_surface);
}
