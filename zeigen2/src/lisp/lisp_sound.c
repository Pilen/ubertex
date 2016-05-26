#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../sound.h"

LISP_BUILTIN(sound, "") {
    if (args -> length > 3 || args -> length < 2) {
        return VALUE_ERROR;
    }

    Value file = LIST_GET_UNSAFE(args, 1);
    if (file.type != STRING) {
        return VALUE_ERROR;
    }

    Int volume = -1;
    if (args -> length > 2) {
        Value volume_v = LIST_GET_UNSAFE(args, 2);
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
    return sound_play(environment, file, volume, 0);
}

LISP_BUILTIN(sound_stop, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }

    Value sound = LIST_GET_UNSAFE(args, 1);
    Int result;
    if (sound.type == SOUND) {
        result = sound_stop(environment, sound.val.sound_val);
    } else if (sound.type == STRING) {
        result = sound_stop_file(environment, sound);
    }
    if (result) {
        return VALUE_INTEGER(result);
    } else {
        return VALUE_NIL;
    }

}

LISP_BUILTIN(sound_stop_all, "") {
    if (args -> length != 1) {
        return VALUE_ERROR;
    }
    sound_stop_all();
    return VALUE_NIL;
}

LISP_BUILTIN(sound_fade_all, "") {
    if (args -> length > 2) {
        return VALUE_ERROR;
    }

    Int duration = 4 * 1000;
    if (args -> length == 2) {
        Value value = LIST_GET_UNSAFE(args, 1);
        if (value.type == INTEGER) {
            duration = value.val.integer_val * 1000;
        } else if (value.type == FLOAT) {
            duration = (Int) (value.val.float_val * 1000.0);
        }
    }

    sound_fade_out_all(duration);
    return VALUE_NIL;
}
