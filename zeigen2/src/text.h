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

Bool text(Environment *environment, String *text, Int fontsize, Bool align_center, Renderable *target);
Bool resource_create_text(Environment *environment, Value skeleton, Unt initial_score, Unt *size);

#endif
