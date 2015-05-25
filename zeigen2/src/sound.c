#include <string.h>

#include "sound.h"
#include "memory.h"
#include "resource.h"
#include "assert.h"
#include "string.h"
#include "file.h"
#include "lock.h"
#include "math.h"
#include "basic.h"
#include "debug.h"

Mutex *sound_lock;
Sound **sound_table;
Int sound_channels;
Int sound_playing;
Int sound_first_free; /* Might not actually be free, but we know nothing is free below */

void sound_finished(Int channel);


void sound_initialize(void) {
    /* Sound and mixer must already be initialized */

    sound_lock = mutex_create();
    sound_channels = OPTION_SOUND_MIXING_CHANNELS;
    sound_table = memory_cmalloc(sizeof(Sound *) * sound_channels);
    sound_playing = 0;
    sound_first_free = 0;
    Mix_ChannelFinished(sound_finished);
}

void sound_table_expand(void) {
    /* Mutexes are reentrant */
    mutex_lock(sound_lock);
    Unt new_channels = sound_channels * 2;
    Sound **new_table = memory_cmalloc(sizeof(Sound *) * new_channels);
    for (Unt i = 0; i < sound_channels; i++) {
        new_table[i] = sound_table[i];
    }
    sound_channels = new_channels;
    sound_table = new_table;
    mutex_unlock(sound_lock);
}

void sound_finished(Int channel) {
    mutex_lock(sound_lock);
    sound_table[channel] = NULL;
    sound_playing--;
    sound_first_free = MIN(sound_first_free, channel);
    mutex_unlock(sound_lock);
}

Value sound_play(Environment *environment, Value filename) {
    Soundsample *skeleton = memory_malloc(sizeof(Soundsample));
    skeleton -> path = filename;
    Value result = resource_get(environment, VALUE_SOUNDSAMPLE(skeleton));
    if (result.type != SOUNDSAMPLE) {
        return VALUE_ERROR;
    }
    Soundsample *soundsample = result.val.soundsample_val;


    mutex_lock(sound_lock);
    if (sound_playing >= sound_channels) {
        sound_table_expand();
    }

    Int channel = sound_first_free;
    while (sound_table[channel] != NULL) {
        channel++;
    }
    z_assert(channel < sound_channels);
    sound_first_free = channel;

    Int actual_channel = Mix_PlayChannel(channel, soundsample -> chunk, 0);
    z_assert(actual_channel == channel);

    Sound *sound = memory_malloc(sizeof(Sound));
    sound -> playing = true;
    sound -> channel = channel;
    sound -> sample = soundsample;
    sound_table[channel] = sound;
    mutex_unlock(sound_lock);
    return VALUE_SOUND(sound);
}

Bool sound_stop(Environment *environment, Sound *sound) {
    /* z_assert(soundv.type == SOUND); */
    /* Sound *sound = soundv.val.sound_val; */

    /* Must lock to avoid NULL pointer referencing if the sound stops concurrently */
    mutex_lock(sound_lock);
    if (!sound -> playing) {
        return false;
    }
    Mix_HaltChannel(sound -> channel);
    mutex_unlock(sound_lock);
    return true;
}

Int sound_stop_file(Environment *environment, Value filename) {
    mutex_lock(sound_lock);
    for (Unt i = 0; i < sound_channels; i++) {
        Sound *sound = sound_table[i];
        if (!sound) {
            continue;
        }
        debug("hej");
        /* NOTE: must match matching used for equal -> SOUNDSAMPLE */
        Value path = sound -> sample -> path;
        if (equal(path, filename)) {
            sound_stop(environment, sound);
        }
    }
    mutex_unlock(sound_lock);
    return 0;
}

Bool soundsample_create(Environment *environment, Value skeleton, Unt initial_score, Unt *size) {
    z_assert(skeleton.type == SOUNDSAMPLE);
    Soundsample *soundsample = skeleton.val.soundsample_val;
    z_assert(soundsample -> path.type == STRING);
    char *filename = soundsample -> path.val.string_val -> text;
    if (strcmp(file_get_extension_str(filename), "mp3") == 0) {
        log_error("conversion from .mp3 to .ogg not yet supported");
    }
    Mix_Chunk *chunk = Mix_LoadWAV(filename);
    if (!chunk) {
        log_error("Could not open file %s. %s", filename, Mix_GetError());
        return false;
    }
    soundsample -> refcount = 0;
    soundsample -> score = initial_score;
    soundsample -> size = chunk -> alen;
    soundsample -> chunk = chunk;
    *size = soundsample -> size;
    return true;
}
