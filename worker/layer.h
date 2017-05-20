#ifndef W_LAYER_H
#define W_LAYER_H

#include "types.h"

typedef struct Layer_s Layer;
struct Layer_s {
    Int index;
    Value entries; /* list */
    Value last_entry; /* last element of entrieslist */
    Layer *next;
};

/* Component definitions are stored as Function values */
struct Component_s {
    Value name; /* symbol */
    Layer *layer; /* Dead if NULL */

    Value local_variables; /* list */

    Value update; /* expression */
    Value background; /* expression */
    Value foreground; /* expression */

    Hash *message_handlers;
};

#endif
