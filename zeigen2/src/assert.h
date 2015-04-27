#ifndef Z_ASSERT_H
#define Z_ASSERT_H

#include "types.h"
#include <stdlib.h>

#ifdef DEBUG
#define assert(v)                                                       \
    do {                                                                \
        Int Z_ASSERT_assert = (v);                                      \
        if (!Z_ASSERT_assert) {                                         \
            printf("ASSERTION-FAILURE: %s:%d: %s: %s gave %d",          \
                   __FILE__, __LINE__, __func__, #v, Z_ASSERT_assert);  \
            exit(EXIT_FAILURE);                                         \
        }                                                               \
    } while (0);
#define assert_build(v)                             \
    do {                                            \
        (void) sizeof(char[1 - 2*!!!(v)]);          \
    } while (0);

#else
#define assert(v)                               \
    do {                                        \
        (void) sizeof((v));                     \
    } while (0);
#define assert_build(v) assert(v)
#endif
#endif
