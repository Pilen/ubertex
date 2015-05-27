#ifndef Z_GRAPHICS_H
#define Z_GRAPHICS_H

#include "types.h"
#include "environment.h"

void graphics_render_at(Environment *environment, SDL_Texture *texture, Int x, Int y);
Bool graphics_render_at_position(Environment *environment, SDL_Texture *texture, Value position);

#endif
