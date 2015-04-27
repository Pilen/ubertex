#ifndef Z_ENVIRONMENT_H
#define Z_ENVIRONMENT_H

#include "hash.h"

typedef struct {
    Hash *variables;
    Hash *functions;
} Environment;

Environment *environment_create(void);

#endif
