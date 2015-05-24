#include <SDL2/SDL.h>
#include <cairo/cairo.h>
#include "graphics.h"

void graphics_render_at(Environment *environment, SDL_Texture *texture, Int x, Int y) {
    SDL_Rect dest;
    dest.x = x;
    dest.y = y;
    SDL_QueryTexture(texture, NULL, NULL, &dest.w, &dest.h);
    SDL_RenderCopy(environment -> renderer, texture, NULL, &dest);
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
    cairo_set_source_rgb(cairo, 0.0, 0.0, 1.0);
    cairo_move_to(cairo, 10.0, 50.0);
    cairo_show_text(cairo, "Hello, World!");

    SDL_Texture *texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface);
    graphics_render_at(environment, texture, 10, 15);

    cairo_destroy(cairo);
    cairo_surface_write_to_png(cairo_surface, "/tmp/hello.png");
    cairo_surface_destroy(cairo_surface);
}
