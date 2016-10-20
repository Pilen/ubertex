#ifndef W_SOUND_H
#define W_SOUND_H

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
Value sound_play(Value filename, Int Volume, Int loops, Environment *environment);

Bool sound_stop(Sound *sound, Environment *environment);
Int sound_stop_file(Value filename, Environment *environment);
void sound_stop_all(void);
void sound_fade_out_all(Int duration);


void sound_mark_dirty(Value filename);

Unt resource_create_soundsample(Value skeleton, Environment *environment);
#endif
