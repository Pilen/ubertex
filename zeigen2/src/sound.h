#ifndef Z_SOUND_H
#define Z_SOUND_H

#include <SDL2/SDL_mixer.h>
#include "types.h"
#include "environment.h"

struct Soundsample_s {
    Unt refcount;
    Unt created;
    Unt last_use;
    Unt size;

    Value path;
    Bool dirty;
    Unt current;
    Mix_Chunk *chunk;
};

struct Sound_s {
    Bool playing;
    Unt channel;
    Soundsample *sample;
};

void sound_initialize(void);
Value sound_play(Environment *environment, Value filename, Int Volume, Int loops);

Bool sound_stop(Environment *environment, Sound *sound);
Int sound_stop_file(Environment *environment, Value filename);
void sound_stop_all(void);
void sound_fade_out_all(Int duration);


void sound_mark_dirty(Value filename);

Bool resource_create_soundsample(Environment *environment, Value skeleton, Unt *size);
#endif
