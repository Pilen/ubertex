
#include <stdio.h>
#include <stdlib.h>
#include <poppler/glib/poppler.h>
#include <cairo.h>
#include <SDL2/SDL.h>

#include "types.h"
#include "pdf.h"
#include "assert.h"
#include "string.h"
#include "debug.h"
#include "graphics.h"

void *pdf_load(Environment *environment, Value skeleton, Unt initial_score) {
    debug("fisk");
    z_assert(skeleton.type == PDF);
    Pdf *pdf = skeleton.val.pdf_val;
    z_assert(skeleton.val.pdf_val -> path.type == STRING);
    GError *error;
    cairo_status_t status;

    char *filename = skeleton.val.pdf_val -> path.val.string_val -> text;
    /* poppler depends on glib's uri implementation instead of a char *filename */
    /* Uris are absolute urls with "file://" prepended */
    /* TODO: decide how much of glib we should depend on, or if we should just accept it and use it all over? */
    gchar *absolute, *uri;
    if (g_path_is_absolute(filename)) {
        absolute = g_strdup(filename);
    } else {
        gchar *dir = g_get_current_dir();
        absolute = g_build_filename(dir, filename, NULL);
        free(dir);
    }
    uri = g_filename_to_uri(absolute, NULL, &error);
    free(absolute);
    if (!uri) {
        log_error("%s", error -> message);
        return NULL;
    }


    char *password = "";
    PopplerDocument *document = poppler_document_new_from_file(uri, password, &error);
    if (!document) {
        log_error("Could not open file %s, %s", uri, error -> message);
        return NULL;
    }

    Int pagecount = poppler_document_get_n_pages(document);

    /* for (Int pagenumber = 0; pagenumber < pagecount; pagenumber++) { */
        PopplerPage *page = poppler_document_get_page(document, 1);
        z_assert(page);

        Double width, height; /* In pixels, beamer default = 12.8cm * 9.6cm at 72 DPI = 362.835000 * 272.126000 */
        poppler_page_get_size(page, &width, &height);

        /* We want to change the resolution from the default 72DPI to PDF_DPI */
        width = PDF_DPI * width / 72.0;
        height = PDF_DPI * height / 72.0;

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
        cairo_scale(cairo, PDF_DPI / 72.0, PDF_DPI / 72.0);
        cairo_save(cairo);
        poppler_page_render(page, cairo); /* Does not fail */
        cairo_restore(cairo);
        g_object_unref(page);

        cairo_set_operator(cairo, CAIRO_OPERATOR_DEST_OVER);
        cairo_set_source_rgb(cairo, 1, 1, 1);
        cairo_paint(cairo);
        status = cairo_status(cairo);
        if (status) {
            log_error("%s", cairo_status_to_string(status));
        }
        cairo_destroy(cairo);
        status = cairo_surface_write_to_png(cairo_surface, "/tmp/test.png");
        if (status) {
            log_error("%s", cairo_status_to_string(status));
        }

        debug("kamel");
        SDL_Texture *texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface);
        z_assert(texture);
        graphics_render_at(environment, texture, 10, 15);

        cairo_surface_destroy(cairo_surface);
        g_object_unref(document);

        pdf -> pagecount = pagecount;
        return NULL;
}
