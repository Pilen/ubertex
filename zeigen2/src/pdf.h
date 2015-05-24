#ifndef Z_PDF_H
#define Z_PDF_H

#include <mupdf/fitz.h>
#include "types.h"

struct Pdf_s {
    Unt refcount;
    float score;
    Value path;
    Unt size;
    Int pagecount;
    fz_context *context;
};

void *pdf_load(Value skeleton, Unt initial_score);

#endif
