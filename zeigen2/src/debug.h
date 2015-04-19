#ifndef Z_DEBUG_H
#define Z_DEBUG_H

/* #ifdef DEBUG */
#include <stdio.h>
#include "zprint.h"

#define debug_value(v) do{printf("DEBUG: %s:%d (print ", __FILE__, __LINE__); print(v); printf(")\n");} while(0)
#define debug(M) printf("DEBUG: %s:%d " #M "\n", __FILE__, __LINE__);
#define debugv(M, ...) printf("DEBUG: %s:%d: " M "\n",__FILE__, __LINE__, ##__VA_ARGS__);
#define debug_type(expression) printf("DEBUG: %d is an int", (expression));
/* #else */
/* #define debug_value(v) do{} while(0) */
/* #define debug(M) ; */
/* #define debugv(M, ...) ; */
/* #define debug_type(expression) ; */

/* #endif */
#endif
