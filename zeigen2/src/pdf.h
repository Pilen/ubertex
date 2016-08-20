#ifndef W_PDF_H
#define W_PDF_H

#include "types.h"
#include "environment.h"

#define PDF_DPI 150.0
#define PDF_RENDER_DPI 72.0

struct Pdf_s {
    Unt refcount;
    Unt created;
    Unt last_use;
    Unt size;

    Value path;
    Int pagecount;
    cairo_surface_t **pages;
};

Bool pdf_get_slide(Environment *environment, Value filename, Int slide, Renderable *target);
Unt resource_create_pdf(Environment *environment, Value skeleton);

#endif
