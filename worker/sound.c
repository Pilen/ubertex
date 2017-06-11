#include <string.h>
#include <sys/stat.h>

#include "headers.h"

Mutex *sound_lock;
Sound **sound_table;
Unt sound_channels;
Unt sound_playing;
Unt sound_first_free; /* Might not actually be free, but we know nothing is free before */

void sound_finished(Int channel);
char *sound_convert_to_ogg(String *mp3);

void sound_initialize(void) {
    /* Sound and mixer must already be initialized */

    sound_lock = mutex_create();
    sound_channels = OPTION_SOUND_MIXING_CHANNELS;
    Mix_AllocateChannels(sound_channels);
    sound_table = memory_cmalloc(sizeof(Sound *) * sound_channels);
    sound_playing = 0;
    sound_first_free = 0;
    Mix_ChannelFinished(sound_finished);
}

void sound_table_expand(void) {
    /* Mutexes are reentrant */
    mutex_lock(sound_lock);
    Unt new_channels = sound_channels * 2;
    Mix_AllocateChannels(sound_channels);
    /* TODO: Use realloc instead? */
    Sound **new_table = memory_cmalloc(sizeof(Sound *) * new_channels);
    for (Unt i = 0; i < sound_channels; i++) {
        new_table[i] = sound_table[i];
    }
    sound_channels = new_channels;
    sound_table = new_table;
    mutex_unlock(sound_lock);
}

void sound_finished(Int channel) {
    w_assert(channel >= 0);
    mutex_lock(sound_lock);
    Soundsample *soundsample = sound_table[channel] -> sample;
    soundsample -> current--;
    if (soundsample -> dirty && soundsample -> current == 0) {
        resource_destroy(VALUE_SOUNDSAMPLE(soundsample));
    }
    sound_table[channel] = NULL;
    sound_playing--;
    sound_first_free = MIN(sound_first_free, (Unt) channel);
    mutex_unlock(sound_lock);
}

Value sound_play(Value filename, Int volume, Int loops, Environment *environment) {
    debug("start");
    Unt start = SDL_GetTicks();
    Soundsample *skeleton = memory_malloc(sizeof(Soundsample));
    skeleton -> path = filename;
    Value result = resource_get(VALUE_SOUNDSAMPLE(skeleton), environment);
    if (result.type != SOUNDSAMPLE) {
        return VALUE_ERROR;
    }
    Soundsample *soundsample = result.val.soundsample_val;

    mutex_lock(sound_lock);
    if (sound_playing >= sound_channels) {
        sound_table_expand();
    }

    Unt channel = sound_first_free;
    while (sound_table[channel] != NULL) {
        channel++;
    }
    w_assert(channel < sound_channels);
    sound_first_free = channel;

    if (volume < 0) {
        volume = MIX_MAX_VOLUME;
    }
    Mix_Volume(channel, volume);

    Int actual_channel = Mix_PlayChannel(channel, soundsample -> chunk, loops);
    w_assert(actual_channel == (Int) channel);

    Sound *sound = memory_malloc(sizeof(Sound));
    sound -> playing = true;
    sound -> channel = channel;
    sound -> sample = soundsample;
    soundsample -> current++;
    sound_table[channel] = sound;
    mutex_unlock(sound_lock);
    Unt done = SDL_GetTicks();
    debugi(done-start);
    debug("done");
    return VALUE_SOUND(sound);
}

Bool sound_stop(Sound *sound, Environment *environment) {
    (void) environment; /* Environment not actually used */
    /* w_assert(soundv.type == SOUND); */
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

Int sound_stop_file(Value filename, Environment *environment) {
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
            sound_stop(sound, environment);
        }
    }
    mutex_unlock(sound_lock);
    return 0;
}

void sound_stop_all(void) {
    Mix_HaltChannel(-1);
}

void sound_fade_out_all(Int duration) {
    Mix_FadeOutChannel(-1, duration);
}

void sound_mark_dirty(Value filename) {
    mutex_lock(sound_lock);
    for (Unt i = 0; i < sound_channels; i++) {
        Sound *sound = sound_table[i];
        if (!sound) {
            continue;
        }
        Value path = sound -> sample -> path;
        if (equal(path, filename)) {
            sound -> sample -> dirty = true;
        }
    }
    mutex_unlock(sound_lock);
}
Unt resource_create_soundsample(Value skeleton, Environment *environment) {
    (void) environment; /* Environment not actually used */
    w_assert(skeleton.type == SOUNDSAMPLE);
    Soundsample *soundsample = skeleton.val.soundsample_val;
    w_assert(soundsample -> path.type == STRING);
    char *filename = soundsample -> path.val.string_val -> text;
    if (strcmp(file_get_extension_str(filename), "mp3") == 0) {
        filename = sound_convert_to_ogg(soundsample -> path.val.string_val);
        log_error("Could not convert %s to ogg file", filename);
        if (!filename) {
            return 0;
        }
    }
    /* Mix_LoadWAV can load WAVE, AIFF, RIFF, OGG, and VOC */
    Mix_Chunk *chunk = Mix_LoadWAV(filename);
    if (!chunk) {
        struct stat file_stat;
        int found = stat(filename, &file_stat);
        if (found == 0) {
            log_error("Found %s but could not read it", filename);
            filename = sound_convert_to_ogg(soundsample -> path.val.string_val);
            if (!filename) {
                log_error("Could not convert %s to ogg file", filename);
                return 0;
            }
            chunk = Mix_LoadWAV(filename);
            if (!chunk) {
                log_error("Could not open converted file %s", filename);
                return 0;
            }
        } else {
            log_error("Could not find file %s", filename);
            return 0;
        }
    }
    soundsample -> refcount = 0;
    soundsample -> created = SDL_GetTicks();
    soundsample -> last_use = soundsample -> created;
    soundsample -> size = chunk -> alen;
    soundsample -> chunk = chunk;
    soundsample -> dirty = false;
    soundsample -> current = 0;
    return soundsample -> size;
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
    Int size = strlen(raw_command) + mp3 -> size - 1 + ogg -> size - 2 * 2 * sizeof(char);
    char *command = memory_cmalloc(sizeof(char) * size);
    Int actual_size = sprintf(command, raw_command, mp3 -> text, ogg -> text);
    w_assert(size == actual_size + 1);
    log_info("Converting %s to .ogg", mp3 -> text);
    Int result = system(command);
    if (result == 0) {
        return ogg -> text;
    } else {
        log_error("Could not convert mp3 to ogg, Failed execution of: %s", command);
        return NULL;
    }
}
