#ifndef Z_COMMUNICATION_H
#define Z_COMMUNICATION_H

#include <SDL2/SDL.h>
#include "list.h"

List *communication_queue;
SDL_mutex *communication_queue_lock;
void communication_initialize(char *host);
#endif
