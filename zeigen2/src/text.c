#include <SDL2/SDL.h>
#include <cairo.h>
#include <pango/pangocairo.h>

#include "text.h"
#include "memory.h"
#include "assert.h"
#include "resource.h"
#include "string.h"
#include "debug.h"

SDL_Texture *text(Environment *environment, String *text, Int fontsize, Bool center) {
    Text *skeleton = memory_malloc(sizeof(Text));
    skeleton -> text = text;
    skeleton -> fontsize = fontsize;
    skeleton -> center = center;
    Value result = resource_get(environment, VALUE_TEXT(skeleton));
    if (result.type == TEXT) {
        return result.val.text_val -> texture;
    } else {
        return NULL;
    }
}

Bool resource_create_text(Environment *environment, Value skeleton, Unt initial_score, Unt *size) {
    /* cairo_status_t status; */
    Int width;
    Int height;

    z_assert(skeleton.type == TEXT);
    Text *text = skeleton.val.text_val;
    (void) text;

    SDL_GetWindowSize(environment -> window, &width, &height);
    SDL_Surface *sdl_surface = SDL_CreateRGBSurface(0, width, height, 32,
                                                    0x00FF0000,
                                                    0x0000FF00,
                                                    0x000000FF,
                                                    0);
    z_assert(sdl_surface);
    cairo_surface_t *cairo_surface;
    cairo_surface = cairo_image_surface_create_for_data(sdl_surface -> pixels,
                                                        CAIRO_FORMAT_RGB24,
                                                        width,
                                                        height,
                                                        sdl_surface -> pitch);
    z_assert(!cairo_surface_status(cairo_surface));
    cairo_t *cairo = cairo_create(cairo_surface);
    z_assert(!cairo_status(cairo));



    PangoLayout *layout = pango_cairo_create_layout(cairo);
    PangoFontDescription *font = pango_font_description_new();
    pango_font_description_set_family(font, "sans");
    pango_font_description_set_style(font, PANGO_STYLE_NORMAL);
    pango_font_description_set_absolute_size(font, text -> fontsize * PANGO_SCALE);
    pango_layout_set_font_description(layout, font);

    debugi(text -> center);
    if (text -> center) {
        pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);
    }
    pango_layout_set_text(layout, text -> text -> text, -1);

    cairo_set_source_rgb(cairo, 1.0, 1.0, 1.0);
    pango_cairo_show_layout(cairo, layout);

    /* cairo_set_operator(cairo, CAIRO_OPERATOR_DEST_OVER); */
    /* cairo_set_source_rgb(cairo, 1, 1, 1); */
    /* cairo_paint(cairo); */
    /* status = cairo_status(cairo); */
    /* if (status) { */
    /*     log_error("%s", cairo_status_to_string(status)); */
    /* } */

    SDL_Texture *texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface);
    z_assert(texture);


    pango_font_description_free(font);
    g_object_unref(layout);

    cairo_destroy(cairo);
    cairo_surface_destroy(cairo_surface);
    SDL_FreeSurface(sdl_surface);


    text -> refcount = 0;
    text -> score = initial_score;
    text -> created = SDL_GetTicks();
    text -> texture = texture;

    text -> size = sizeof(Unt) * width * height; /* Approximate size of texture */
    *size = text -> size;

    return true;
}
