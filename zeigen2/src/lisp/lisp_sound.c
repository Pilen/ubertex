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
    if (args -> length != 2) {
        return VALUE_ERROR;
    }

    Value file = LIST_GET_UNSAFE(args, 1);
    if (file.type != STRING) {
        return VALUE_ERROR;
    }
    return sound_play(environment, file);
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
