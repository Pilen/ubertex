#include "../headers.h"

LISP_BUILTIN(sound, "") {
    ENSURE_NOT_EMPTY(args);
    Value file = NEXT(args);
    if (file.type != STRING) {
        return VALUE_ERROR;
    }

    Int volume = -1;
    if (args.type == CONS) {
        Value volume_v = NEXT(args);
        switch (volume_v.type) {
        case INTEGER:
            volume = volume_v.val.integer_val;
            break;
        case NIL:
            volume = -1;
            break;
        default:
            return VALUE_ERROR;
        }
    }
    return sound_play(file, volume, 0, environment);
}

LISP_BUILTIN(sound_stop, "") {
    ENSURE_NOT_EMPTY(args);
    Value sound = NEXT(args);
    Int result = 0;
    if (sound.type == SOUND) {
        result = sound_stop(sound.val.sound_val, environment);
    } else if (sound.type == STRING) {
        result = sound_stop_file(sound, environment);
    }
    if (result) {
        return VALUE_INTEGER(result);
    } else {
        return VALUE_NIL;
    }

}

LISP_BUILTIN(sound_stop_all, "") {
    sound_stop_all();
    ENSURE_EMPTY(args);
    return VALUE_NIL;
}

LISP_BUILTIN(sound_fade_all, "") {
    Int duration = 4 * 1000;
    if (args.type == CONS) {
        Value value = NEXT(args);
        if (value.type == INTEGER) {
            duration = value.val.integer_val * 1000;
        } else if (value.type == FLOAT) {
            duration = (Int) (value.val.float_val * 1000.0);
        }
    }
    sound_fade_out_all(duration);
    ENSURE_EMPTY(args);
    return VALUE_NIL;
}
