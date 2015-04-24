#ifndef Z_DEBUG_H
#define Z_DEBUG_H

#ifdef DEBUG
#include <stdio.h>
#include "print.h"

#define debug_value(v) do{printf("DEBUG: %s:%d (print ", __FILE__, __LINE__); print((v)); printf(")\n");} while (0);
#define debugr(M) do{printf("DEBUG: %s:%d " #M "\n", __FILE__, __LINE__);} while (0);
#define debug(...) do{printf("DEBUG: %s:%d: ", __FILE__, __LINE__); printf(__VA_ARGS__); printf("\n");} while (0);
#define debug_type(expression) do{printf("DEBUG: %d is an int", (expression));} while (0);
#else
#define debug_value(v) do{} while (0);
#define debug(M) do{} while (0);
#define debugv(M, ...) do{} while (0);
#define debug_type(expression) do{} while (0);

#endif
#endif
