#ifndef Z_SOUND_H
#define Z_SOUND_H

#include <SDL2/SDL_mixer.h>
#include "types.h"
#include "environment.h"

struct Soundsample_s {
    Unt refcount;
    float score;
    Value path;
    Unt size;
    Mix_Chunk *chunk;
};

struct Sound_s {
    Bool playing;
    Unt channel;
    Soundsample *sample;
};

void sound_initialize(void);
Value sound_play(Environment *environment, Value filename);

Bool sound_stop(Environment *environment, Sound *sound);
Int sound_stop_file(Environment *environment, Value filename);

Bool soundsample_create(Environment *environment, Value skeleton, Unt initial_score, Unt *size);
#endif