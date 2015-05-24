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
    SDL_Texture **pages;
};

SDL_Texture *pdf_get_slide(Environment *environment, Value filename, Int slide);

Bool pdf_create(Environment *environment, Value skeleton, Unt initial_score, Unt *size);

#endif
