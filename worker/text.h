#ifndef W_TEXT_H
#define W_TEXT_H

#include "types.h"
#include "environment.h"

typedef enum {
    text_alignment_center,
    text_alignment_left,
    text_alignment_right,
} Text_alignment;

struct Text_s {
    Unt refcount;
    Unt created;
    Unt size;
    String *text;
    Int fontsize;
    Bool center;
    SDL_Texture *texture;
};

Bool text(String *text, Int fontsize, Text_alignment alignment, Renderable *target, Environment *environment);

#endif
