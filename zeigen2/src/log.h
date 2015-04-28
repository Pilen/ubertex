#ifndef Z_LOG_H
#define Z_LOG_H

#include <stdlib.h>
#include <stdio.h>

#define LOG_LEVEL_MAX 127
int log_level;

#define LOG(STATUS, level, ...)                        \
    do {                                               \
        if (level <= log_level) {                      \
            printf(#STATUS ": ");                      \
            printf(__VA_ARGS__);                       \
            printf("\n");                              \
        }                                              \
    } while (0);
#define log_error_in (5 <= log_level ? printf("ERROR-IN: %s:%d: %s\n", __FILE__, __LINE__, __func__): 0)
#define log_info(...) LOG(INFO, 4, __VA_ARGS__)
#define log_warning(...) LOG(WARNING, 3, __VA_ARGS__)
#define log_error(...) LOG(ERROR, 2, __VA_ARGS__)
#define log_fatal(...)                          \
    do {                                        \
        LOG(FATAL-ERROR, 1, __VA_ARGS__);       \
        if (4 <= log_level) {                   \
            printf("exiting...\n");           \
        }                                       \
        exit(EXIT_FAILURE);                     \
    } while (0);

#endif
