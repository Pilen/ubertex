#include <SDL2/SDL.h>
#include "graphics.h"

void graphics_render_at(Environment *environment, SDL_Texture *texture, Int x, Int y) {
    SDL_Rect dest;
    dest.x = x;
    dest.y = y;
    SDL_QueryTexture(texture, NULL, NULL, &dest.w, &dest.h);
    SDL_RenderCopy(environment -> renderer, texture, NULL, &dest);
}
