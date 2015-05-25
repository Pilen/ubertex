
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
#include "memory.h"
#include "resource.h"
#include "file.h"

SDL_Texture *pdf_get_slide(Environment *environment, Value filename, Int slide) {
    Pdf *skeleton = memory_malloc(sizeof(Pdf));
    skeleton -> path = filename;
    Value result = resource_get(environment, VALUE_PDF(skeleton));

    if (result.type == PDF) {
        Pdf *pdf = result.val.pdf_val;
        if (slide >= 0 && slide < pdf -> pagecount) {
            return pdf -> pages[slide];
        }
    }
    return NULL;
}

Bool pdf_create(Environment *environment, Value skeleton, Unt initial_score, Unt *size) {
    debug("fisk");
    cairo_status_t status;
    z_assert(skeleton.type == PDF);
    Pdf *pdf = skeleton.val.pdf_val;
    z_assert(pdf -> path.type == STRING);

    char *password = "";
    char *filename = pdf -> path.val.string_val -> text;

    char *buffer;
    size_t buffer_size;
    Bool found = file_read_raw(filename, &buffer, &buffer_size);
    if (!found) {
        log_error("Could not open file %s", filename);
        return false;
    }

    PopplerDocument *document = poppler_document_new_from_data(buffer, buffer_size, password, NULL);
    if (!document) {
        log_error("Not a valid pdf file %s", filename);
        return false;
    }

    Unt size_sum = 0;
    Int pagecount = poppler_document_get_n_pages(document);
    SDL_Texture **pages = memory_malloc(sizeof(SDL_Texture *) * pagecount);

    /* Load all the pages */
    for (Int pagenumber = 0; pagenumber < pagecount; pagenumber++) {
        PopplerPage *page = poppler_document_get_page(document, pagenumber);
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

        SDL_Texture *texture = SDL_CreateTextureFromSurface(environment -> renderer, sdl_surface);
        z_assert(texture);
        pages[pagenumber] = texture;

        cairo_destroy(cairo);
        cairo_surface_destroy(cairo_surface);

        size_sum += sizeof(Unt) * width * height; /* Approximate size of texture */
    }
    debug("done");
    g_object_unref(document);

    pdf -> refcount = 0;
    pdf -> score = initial_score;
    pdf -> size = size_sum;
    pdf -> pagecount = pagecount;
    pdf -> pages = pages;

    *size = size_sum;

    return pdf;
}
