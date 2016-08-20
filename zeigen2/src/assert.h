#ifndef W_ASSERT_H
#define W_ASSERT_H

#include "types.h"
#include <stdlib.h>
#include "options.h"

#if OPTION_DEBUG
#define w_assert(v)                             \
    do {                                        \
        Int W_ASSERT_assert = !!(v);            \
        if (!W_ASSERT_assert) {                 \
            log_assert(#v, W_ASSERT_assert);    \
            exit(EXIT_FAILURE);                 \
        }                                       \
    } while (0);
#define assert_build(v)                         \
    do {                                        \
        (void) sizeof(char[1 - 2*!!!(v)]);      \
    } while (0);


#else
#define w_assert(v)                             \
    do {                                        \
        (void) sizeof((v));                     \
    } while (0);
#define assert_build(v) w_assert(v)
#endif
#endif
