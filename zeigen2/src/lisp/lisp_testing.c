#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../graphics.h"

LISP_BUILTIN(allocate_useless, "") {
    size_t size = 10000;
    Unt *mem = memory_malloc(sizeof(Unt) * size);
    Unt i;
    for (i = 0; i < size; i++) {
        mem[i] = i;
    }
    return VALUE_INTEGER(i);
}

LISP_BUILTIN(render_test, "") {
    void graphics_cairo_test(Environment *environment);
    graphics_cairo_test(environment);
    return VALUE_NIL;
}
