#include <string.h>
#include <sys/stat.h>

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
char *sound_convert_to_ogg(String *mp3);

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

Value sound_play(Environment *environment, Value filename, Int volume, Int loops) {
    debug("start");
    Unt start = SDL_GetTicks();
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

    if (volume < 0) {
        volume = MIX_MAX_VOLUME;
    }
    Mix_Volume(channel, volume);

    Int actual_channel = Mix_PlayChannel(channel, soundsample -> chunk, loops);
    z_assert(actual_channel == channel);

    Sound *sound = memory_malloc(sizeof(Sound));
    sound -> playing = true;
    sound -> channel = channel;
    sound -> sample = soundsample;
    sound_table[channel] = sound;
    mutex_unlock(sound_lock);
    Unt done = SDL_GetTicks();
    debugi(done-start);
    debug("done");
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

void sound_stop_all(void) {
    Mix_HaltChannel(-1);
}

Bool resource_create_soundsample(Environment *environment, Value skeleton, Unt initial_score, Unt *size) {
    z_assert(skeleton.type == SOUNDSAMPLE);
    Soundsample *soundsample = skeleton.val.soundsample_val;
    z_assert(soundsample -> path.type == STRING);
    char *filename = soundsample -> path.val.string_val -> text;
    if (strcmp(file_get_extension_str(filename), "mp3") == 0) {
        filename = sound_convert_to_ogg(soundsample -> path.val.string_val);
        if (!filename) {
            return false;
        }
    }
    Mix_Chunk *chunk = Mix_LoadWAV(filename);
    if (!chunk) {
        struct stat file_stat;
        Bool found = stat(filename, &file_stat);
        if (found == 0) {
            log_error("Found %s but could not read it", filename);
            filename = sound_convert_to_ogg(soundsample -> path.val.string_val);
            if (!filename) {
                return false;
            }
            chunk = Mix_LoadWAV(filename);
            if (!chunk) {
                log_error("Could not open converted file %s", filename);
                return false;
            }
        } else {
            log_error("Could not find file %s", filename);
            return false;
        }
    }
    soundsample -> refcount = 0;
    soundsample -> score = initial_score;
    soundsample -> size = chunk -> alen;
    soundsample -> chunk = chunk;
    *size = soundsample -> size;
    return true;
}

char *sound_convert_to_ogg(String *mp3) {
    String *ogg = string_concatenate(mp3, string_create_from_str(".ogg"));

    /* Determine if the ogg is newer than the mp3, if so no need to convert */
    struct stat mp3_stat;
    struct stat ogg_stat;
    Int found;
    found = stat(mp3 -> text, &mp3_stat);
    if (found != 0) {
        log_error("Could not find file %s", mp3 -> text);
        return NULL;
    }
    long mp3_time = mp3_stat.st_mtime;

    found = stat(ogg -> text, &ogg_stat);
    if (found == 0) {
        long ogg_time = ogg_stat.st_mtime;
        if (ogg_time > mp3_time) {
            return ogg -> text;
        }
    }

    char *raw_command = "ffmpeg -loglevel quiet -i \"%s\" -c:a libvorbis -q:a 7 -vn -y \"%s\"";
    Unt size = strlen(raw_command) + mp3 -> size - 1 + ogg -> size - 2 * 2 * sizeof(char);
    char *command = memory_cmalloc(sizeof(char) * size);
    Int actual_size = sprintf(command, raw_command, mp3 -> text, ogg -> text);
    z_assert(size == actual_size + 1);
    log_info("Converting %s to .ogg", mp3 -> text);
    Int result = system(command);
    if (result == 0) {
        return ogg -> text;
    } else {
        log_error("Could not convert mp3 to ogg, Failed execution of: %s", command);
        return NULL;
    }
}
