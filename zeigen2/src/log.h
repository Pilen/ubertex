#ifndef Z_LOG_H
#define Z_LOG_H

#include <stdlib.h>
#include <stdio.h>

#define LOG(STATUS, ...)                               \
    do {                                               \
        printf(#STATUS ": ");                          \
        printf(__VA_ARGS__);                           \
        printf("\n");                                  \
    } while (0);
#define log_error(...) LOG(ERROR, __VA_ARGS__)
#define log_warning(...) LOG(WARNING, __VA_ARGS__)
#define log_info(...) LOG(INFO, __VA_ARGS__)
#define log_fatal(...)                          \
    do {                                        \
        LOG(FATAL-ERROR, __VA_ARGS__);          \
        printf("\nexiting...\n");               \
        exit(EXIT_FAILURE);                     \
    } while (0);

#endif
