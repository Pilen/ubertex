#ifndef Z_TEXT_H
#define Z_TEXT_H

#include "types.h"
#include "environment.h"

struct Text_s {
    Unt refcount;
    float score;
    String *text;
    Int fontsize;
    Bool center;
    Unt size;
    SDL_Texture *texture;
};

SDL_Texture *text(Environment *environment, String *text, Int fontsize, Bool center);
Bool text_create(Environment *environment, Value skeleton, Unt initial_score, Unt *size);

#endif
