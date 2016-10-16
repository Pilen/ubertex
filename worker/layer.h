#ifndef W_LAYER_H
#define W_LAYER_H

#include "types.h"

typedef struct Layer_s Layer;
struct Layer_s {
    Int index;
    Value entries; /* list */
    Value last_entry; /* last element of entrieslist */
    Layer *lower;
    Layer *higher;
};

/* Component definitions are stored as Function values */
struct Component_s {
    Value name; /* symbol */

    Value local_variables; /* list */

    Value update; /* body */
    Value update_arguments; /* list */

    Value render; /* body */
    Value render_arguments; /* list */

    Value message_queue; /* list? */
};

#endif
