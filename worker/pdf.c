#include <stdio.h>
#include <stdlib.h>
#include <poppler/glib/poppler.h>
#include <cairo.h>
#include <SDL2/SDL.h>

#include "headers.h"

Bool pdf_get_slide(Value filename, Int slide, Renderable *target, Environment *environment) {
    Pdf *skeleton = NEW(Pdf);
    skeleton -> path = filename;
    Value result = resource_get(VALUE_PDF(skeleton), environment);

    if (result.type == PDF) {
        Pdf *pdf = result.val.pdf_val;
        if (slide >= 0 && slide < pdf -> pagecount) {
            cairo_surface_t *surface = pdf -> pages[slide];
            target -> data = (void *) surface;
            target -> render = graphics_show_cairo_surface;
            target -> width = cairo_image_surface_get_width(surface);
            target -> height = cairo_image_surface_get_height(surface);
            return true;
        }
    }
    return false;
}

Unt resource_create_pdf(Value skeleton, Environment *environment) {
    (void) environment; /* Environment not actually used */
    debug("Loading pdf: %s", skeleton.val.pdf_val -> path.val.string_val -> text);
    cairo_status_t status;
    w_assert(skeleton.type == PDF);
    Pdf *pdf = skeleton.val.pdf_val;
    w_assert(pdf -> path.type == STRING);

    char *password = "";
    char *filename = pdf -> path.val.string_val -> text;

    /* PDF files are loaded manually to avoid the uri system of poppler */
    char *buffer;
    size_t buffer_size;
    Bool found = file_read_raw(filename, &buffer, &buffer_size);
    if (!found) {
        log_error("Could not open file %s", filename);
        return 0;
    }

    PopplerDocument *document = poppler_document_new_from_data(buffer, buffer_size, password, NULL);
    if (!document) {
        log_error("Not a valid pdf file %s", filename);
        return 0;
    }

    Unt size = 0;
    Int pagecount = poppler_document_get_n_pages(document);
    cairo_surface_t **pages = NEW_BUFFER(cairo_surface_t *, pagecount);

    /* Load all the pages */
    for (Int pagenumber = 0; pagenumber < pagecount; pagenumber++) {
        PopplerPage *page = poppler_document_get_page(document, pagenumber);
        w_assert(page);

        Double width, height; /* In pixels, beamer default = 12.8cm * 9.6cm at 72 DPI = 362.835000 * 272.126000 */
        poppler_page_get_size(page, &width, &height);

        /* We want to change the resolution from the default 72DPI to PDF_DPI */
        /* width = PDF_DPI * width / PDF_RENDER_DPI; */
        /* height = PDF_DPI * height / PDF_RENDER_DPI; */
        double scale = PDF_DPI/PDF_RENDER_DPI + 1;
        width *= scale;
        height *= scale;

        cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,
                                                              width, height);
        w_assert(!cairo_surface_status(surface));
        cairo_t *cairo = cairo_create(surface);
        w_assert(!cairo_status(cairo));
        cairo_scale(cairo, scale, scale);
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

        cairo_surface_flush(surface);
        pages[pagenumber] = surface;

        cairo_destroy(cairo);
        /* cairo_surface_destroy(cairo_surface); */

        size += sizeof(Unt) * width * height; /* Approximate size of texture */
    }
    g_object_unref(document);

    pdf -> refcount = 0;
    pdf -> created = SDL_GetTicks();
    pdf -> last_use = pdf -> created;
    pdf -> size = size;
    pdf -> pagecount = pagecount;
    pdf -> pages = pages;

    return size;
}
