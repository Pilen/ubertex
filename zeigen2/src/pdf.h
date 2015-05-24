#ifndef Z_PDF_H
#define Z_PDF_H

#include <mupdf/fitz.h>
#include "types.h"
#include "environment.h"

#define PDF_DPI 150
struct Pdf_s {
    Unt refcount;
    float score;
    Value path;
    Unt size;
    Int pagecount;
    SDL_Texture *pages;
    fz_context *context;
};

void *pdf_load(Environment *environment, Value skeleton, Unt initial_score);

#endif
