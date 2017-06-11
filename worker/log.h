#ifndef W_LOG_H
#define W_LOG_H

#include <stdlib.h>
#include <stdio.h>

#define LOG_LEVEL_MAX 127
int log_level;
FILE *log_output;

#define LOG(STATUS, level, ...)                                     \
    do {                                                            \
        if (level <= log_level) {                                   \
            fprintf(log_output, #STATUS ": ");                      \
            fprintf(log_output, __VA_ARGS__);                       \
            fprintf(log_output, "\n");                              \
            fflush(log_output);                                     \
        }                                                           \
    } while (0);

#define log_section(...) LOG(SECTION, 12, __VA_ARGS__)
#define log_calloc(amount, size) (11 <= log_level? fprintf(log_output, "CALLOC: %s:%d: \t%s \t%d * %s = %d * %zd\n", __FILE__, __LINE__, __func__, amount, #size, amount, (size_t) (size)) : 0)
#define log_malloc(size) (10 <= log_level ? fprintf(log_output, "MALLOC: %s:%d: \t%s \t%s = %zd\n", __FILE__, __LINE__, __func__, #size, (size_t) (size)) : 0)
#define log_resource(TYPE, size) (9  <= log_level ? fprintf(log_output, "RESOURCE: %s size %d", #TYPE, size) : 0)
#define log_error_in (8 <= log_level ? fprintf(log_output, "ERROR-IN: %s:%d: %s\n", __FILE__, __LINE__, __func__): 0)
#define log_info(...) LOG(INFO, 7, __VA_ARGS__)
#define log_warning(...) LOG(WARNING, 6, __VA_ARGS__)
#define log_error(...) LOG(ERROR, 5, __VA_ARGS__)
#define log_user(...) LOG(USER, 4, __VA_ARGS__)
#define log_late(time) LOG(LATE, 3, "%d", time)
#define log_assert(v, ve) LOG(ASSERTION-FAILURE, 2, "%s:%d: %s: \t%s gave %d", __FILE__, __LINE__, __func__, #v, ve)
#define log_fatal(...)                                   \
    do {                                                 \
        if (1 <= log_level) {                            \
            fprintf(log_output, "FATAL-ERROR: %s:%d:%s: ", __FILE__, __LINE__, __func__); \
            fprintf(log_output, __VA_ARGS__);            \
            fprintf(log_output, "\nexiting...\n");       \
        }                                                \
        exit(EXIT_FAILURE);                              \
    } while (0);


FILE *log_initialize_file(void);

#define LOG_DISABLE() int log_stored_value ## __FUNC__ = log_level; log_level = 0;
#define LOG_REENABLE() log_level = log_stored_value ## __FUNC__;

#endif
