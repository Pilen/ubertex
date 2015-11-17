#ifndef Z_TEXT_H
#define Z_TEXT_H

#include "types.h"
#include "environment.h"

struct Text_s {
    Unt refcount;
    float score;
    Unt size;
    Unt created;
    String *text;
    Int fontsize;
    Bool center;
    SDL_Texture *texture;
};

SDL_Texture *text(Environment *environment, String *text, Int fontsize, Bool center);
Bool resource_create_text(Environment *environment, Value skeleton, Unt initial_score, Unt *size);

#endif
