#include <SDL2/SDL.h>
#include <cairo.h>
#include <pango/pangocairo.h>
#include "libs/cairosdl/cairosdl.h"

#include "text.h"
#include "memory.h"
#include "assert.h"
#include "resource.h"
#include "string.h"
#include "debug.h"
#include "graphics.h"

void text_show(void *data, Environment *environment) {
    PangoLayout *layout = (PangoLayout *) data;
    pango_cairo_show_layout(environment -> cairo, layout);
    g_object_unref(layout);
}

Bool text(String *text, Int fontsize, Bool align_center, Renderable *target, Environment *environment) {
    if (fontsize <= 0) {
        fontsize = 26;
    }
    PangoLayout *layout;
    layout = pango_cairo_create_layout(environment -> cairo);

    PangoFontDescription *font_description;
    font_description = pango_font_description_new();
    pango_font_description_set_family(font_description, "sans");
    pango_font_description_set_style(font_description, PANGO_STYLE_NORMAL);
    pango_font_description_set_absolute_size(font_description, fontsize * PANGO_SCALE);
    pango_layout_set_font_description(layout, font_description);
    /* Can be freed already according to pango official example */
    pango_font_description_free(font_description);


    pango_layout_set_markup(layout, text -> text, -1);
    if (align_center) {
        pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);
    }

    /* assume position already set */
    Int width, height;
    pango_layout_get_pixel_size(layout, &width, &height);
    target -> data = (void *) layout;
    target -> render = &text_show;
    target -> width = width;
    target -> height = height;
    return true;
}

/* SDL_Texture *text(String *text, Int fontsize, Bool center, Environment *environment) { */
/*     Text *skeleton = memory_malloc(sizeof(Text)); */
/*     skeleton -> text = text; */
/*     skeleton -> fontsize = fontsize; */
/*     skeleton -> center = center; */
/*     Value result = resource_get(VALUE_TEXT(skeleton), environment); */
/*     if (result.type == TEXT) { */
/*         return result.val.text_val -> texture; */
/*     } else { */
/*         return NULL; */
/*     } */
/* } */

/* Bool resource_create_text(Value skeleton, Unt initial_score, Unt *size, Environment *environment) { */
/*     debug("Creating text"); */
/*     /\* cairo_status_t status; *\/ */
/*     Int width; */
/*     Int height; */
/*     SDL_GetWindowSize(environment -> window, &width, &height); */

/*     w_assert(skeleton.type == TEXT); */
/*     Text *text = skeleton.val.text_val; */

/*     SDL_Surface *sdl_surface = SDL_CreateRGBSurface(0, width, height, 32, */
/*                                                     0x00FF0000, */
/*                                                     0x0000FF00, */
/*                                                     0x000000FF, */
/*                                                     0); */
/*     w_assert(sdl_surface); */
/*     cairo_surface_t *cairo_surface; */
/*     cairo_surface = cairo_image_surface_create_for_data(sdl_surface -> pixels, */
/*                                                         CAIRO_FORMAT_RGB24, */
/*                                                         width, */
/*                                                         height, */
/*                                                         sdl_surface -> pitch); */
/*     w_assert(!cairo_surface_status(cairo_surface)); */
/*     cairo_t *cairo = cairo_create(cairo_surface); */
/*     w_assert(!cairo_status(cairo)); */
/*     PangoLayout *layout = pango_cairo_create_layout(cairo); */
/*     PangoFontDescription *font = pango_font_description_new(); */
/*     pango_font_description_set_family(font, "sans"); */
/*     pango_font_description_set_style(font, PANGO_STYLE_NORMAL); */
/*     pango_font_description_set_absolute_size(font, text -> fontsize * PANGO_SCALE); */
/*     pango_layout_set_font_description(layout, font); */
/*     if (text -> center) { */
/*         pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER); */
/*     } */
/*     cairo_set_source_rgb(cairo, 1.0, 1.0, 1.0); */
/*     pango_layout_set_text(layout, text -> text -> text, -1); */
/*     pango_cairo_show_layout(cairo, layout); */
/*     int text_width, text_height; */
/*     pango_layout_get_pixel_size(layout, &text_width, &text_height); */

/*     /\* cairo_set_operator(cairo, CAIRO_OPERATOR_DEST_OVER); *\/ */
/*     /\* cairo_set_source_rgb(cairo, 1, 1, 1); *\/ */
/*     /\* cairo_paint(cairo); *\/ */
/*     /\* status = cairo_status(cairo); *\/ */
/*     /\* if (status) { *\/ */
/*     /\*     log_error("%s", cairo_status_to_string(status)); *\/ */
/*     /\* } *\/ */

/*     SDL_Texture *uncropped_texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface); */
/*     w_assert(uncropped_texture); */


/*     Unt format; */
/*     SDL_QueryTexture(uncropped_texture, &format, NULL, NULL, NULL); */

/*     SDL_Texture *texture = SDL_CreateTexture(environment -> renderer, format, SDL_TEXTUREACCESS_TARGET, text_width, text_height); */
/*     SDL_SetRenderTarget(environment -> renderer, texture); */
/*     SDL_Rect rect; */
/*     rect.x = 0; */
/*     rect.y = 0; */
/*     rect.w = text_width; */
/*     rect.h = text_height; */
/*     SDL_RenderCopy(environment -> renderer, uncropped_texture, &rect, &rect); */
/*     SDL_SetRenderTarget(environment -> renderer, NULL); */

/*     SDL_DestroyTexture(uncropped_texture); */
/*     pango_font_description_free(font); */
/*     g_object_unref(layout); */
/*     cairo_destroy(cairo); */
/*     cairo_surface_destroy(cairo_surface); */
/*     SDL_FreeSurface(sdl_surface); */

/*     text -> refcount = 0; */
/*     text -> score = initial_score; */
/*     text -> created = SDL_GetTicks(); */
/*     text -> texture = texture; */

/*     text -> size = sizeof(Unt) * width * height; /\* Approximate size of texture *\/ */
/*     *size = text -> size; */

/*     return true; */
/* } */

/* SDL_Texture *text_test(Environment *environment) { */
/*     Int width; */
/*     Int height; */
/*     Int fontsize = 24; */
/*     SDL_GetWindowSize(environment -> window, &width, &height); */


/*     SDL_Surface *sdl_surface = SDL_CreateRGBSurface(0, width, height, 32, */
/*                                                     0x00FF0000, */
/*                                                     0x0000FF00, */
/*                                                     0x000000FF, */
/*                                                     0); */

/*     cairo_surface_t *cairo_surface = cairo_image_surface_create_for_data(sdl_surface -> pixels, */
/*                                                                          CAIRO_FORMAT_RGB24, */
/*                                                                          width, */
/*                                                                          height, */
/*                                                                          sdl_surface -> pitch); */
/*     cairo_t *cairo = cairo_create(cairo_surface); */
/*     PangoLayout *layout = pango_cairo_create_layout(cairo); */
/*     PangoFontDescription *font = pango_font_description_new(); */
/*     pango_font_description_set_family(font, "sans"); */
/*     pango_font_description_set_style(font, PANGO_STYLE_NORMAL); */
/*     pango_font_description_set_absolute_size(font, fontsize * PANGO_SCALE); */
/*     pango_layout_set_font_description(layout, font); */
/*     /\* cairo_select_font_face(cairo, "serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD); *\/ */
/*     /\* cairo_set_font_size(cairo, 32.0); *\/ */
/*     /\* cairo_set_source_rgb(cairo, 0.0, 1.0, 1.0); *\/ */
/*     /\* cairo_move_to(cairo, 10.0, 50.0); *\/ */
/*     /\* cairo_show_text(cairo, "Hello, World!"); *\/ */
/*     pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER); */
/*     cairo_set_source_rgb(cairo, 1.0, 1.0, 1.0); */
/*     pango_layout_set_text(layout, "Hello,\n World!", -1); */
/*     pango_cairo_show_layout(cairo, layout); */
/*     debugi(pango_layout_get_width(layout)); */
/*     debugi(pango_layout_get_height(layout)); */
/*     int pw, ph; */
/*     pango_layout_get_pixel_size(layout, &pw, &ph); */
/*     debugi(pw); */
/*     debugi(ph); */
/*     debugi(pango_layout_get_line_count(layout)); */
/*     debugi(pango_layout_get_spacing(layout)); */


/*     SDL_Texture *texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface); */
/*     /\* graphics_render_at(texture, 10, 15, environment); *\/ */

/*     pango_font_description_free(font); */
/*     g_object_unref(layout); */
/*     cairo_destroy(cairo); */
/*     cairo_surface_write_to_png(cairo_surface, "/tmp/hello.png"); */
/*     cairo_surface_destroy(cairo_surface); */


/*     Unt format; */
/*     SDL_QueryTexture(texture, &format, NULL, NULL, NULL); */
/*     SDL_Texture *final = SDL_CreateTexture(environment -> renderer, format, SDL_TEXTUREACCESS_TARGET, pw, ph); */
/*     SDL_SetRenderTarget(environment -> renderer, final); */
/*     SDL_Rect rect; */
/*     rect.x = 0; */
/*     rect.y = 0; */
/*     rect.w = pw; */
/*     rect.h = ph; */
/*     SDL_RenderCopy(environment -> renderer, texture, &rect, &rect); */
/*     SDL_SetRenderTarget(environment -> renderer, NULL); */
/*     SDL_DestroyTexture(texture); */

/*     return final; */
/* } */

/* SDL_Texture *text_test2(Environment *environment) { */
/*     Int width; */
/*     Int height; */
/*     Int fontsize = 24; */
/*     SDL_GetWindowSize(environment -> window, &width, &height); */

/*     SDL_Surface *sdl_surface = SDL_CreateRGBSurface(0, width, height, 32, */
/*                                                     0x00FF0000, */
/*                                                     0x0000FF00, */
/*                                                     0x000000FF, */
/*                                                     0xFF000000 */
/*                                                     /\* 0 *\/ */
/*                                                     ); */

/*     cairo_surface_t *cairo_surface = cairosdl_surface_create(sdl_surface); */

/*     cairo_t *cairo = cairo_create(cairo_surface); */
/*     PangoLayout *layout = pango_cairo_create_layout(cairo); */
/*     PangoFontDescription *font = pango_font_description_new(); */
/*     pango_font_description_set_family(font, "sans"); */
/*     pango_font_description_set_style(font, PANGO_STYLE_NORMAL); */
/*     pango_font_description_set_absolute_size(font, fontsize * PANGO_SCALE); */
/*     pango_layout_set_font_description(layout, font); */
/*     pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER); */
/*     cairo_set_source_rgb(cairo, 1.0, 1.0, 1.0); */
/*     pango_layout_set_text(layout, "Hello,\n World!", -1); */
/*     pango_cairo_show_layout(cairo, layout); */
/*     int pw, ph; */
/*     pango_layout_get_pixel_size(layout, &pw, &ph); */

/*     cairosdl_surface_flush(cairo_surface); */

/*     SDL_Texture *texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface); */

/*     /\* cairosdl_surface_flush(cairo_surface); *\/ */
/*     /\* pango_font_description_free(font); *\/ */
/*     /\* g_object_unref(layout); *\/ */
/*     /\* cairo_destroy(cairo); *\/ */
/*     /\* cairo_surface_destroy(cairo_surface); *\/ */

/*     Unt format; */
/*     SDL_QueryTexture(texture, &format, NULL, NULL, NULL); */
/*     SDL_Texture *final = SDL_CreateTexture(environment -> renderer, format, SDL_TEXTUREACCESS_TARGET, pw, ph); */
/*     SDL_SetRenderTarget(environment -> renderer, final); */
/*     SDL_Rect rect; */
/*     rect.x = 0; */
/*     rect.y = 0; */
/*     rect.w = pw; */
/*     rect.h = ph; */
/*     /\* cairosdl_surface_flush(cairo_surface); *\/ */

/*     SDL_RenderCopy(environment -> renderer, texture, &rect, &rect); */
/*     SDL_SetRenderTarget(environment -> renderer, NULL); */
/*     SDL_DestroyTexture(texture); */



/*     return final; */
/* } */
