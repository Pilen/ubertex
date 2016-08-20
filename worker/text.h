#ifndef W_TEXT_H
#define W_TEXT_H

#include "types.h"
#include "environment.h"

struct Text_s {
    Unt refcount;
    Unt created;
    Unt size;
    String *text;
    Int fontsize;
    Bool center;
    SDL_Texture *texture;
};

Bool text(Environment *environment, String *text, Int fontsize, Bool align_center, Renderable *target);

#endif
