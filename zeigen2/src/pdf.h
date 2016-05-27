#ifndef Z_PDF_H
#define Z_PDF_H

#include "types.h"
#include "environment.h"

#define PDF_DPI 150.0
#define PDF_RENDER_DPI 72.0

struct Pdf_s {
    Unt refcount;
    float score;
    Value path;
    Unt size;
    Unt created;
    Int pagecount;
    cairo_surface_t **pages;
};

Bool pdf_get_slide(Environment *environment, Value filename, Int slide, Renderable *target);
Bool resource_create_pdf(Environment *environment, Value skeleton, Unt initial_score, Unt *size);

#endif
