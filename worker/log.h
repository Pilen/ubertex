#ifndef W_LOG_H
#define W_LOG_H

#include <stdlib.h>
#include <stdio.h>

#define LOG_LEVEL_MAX 127
int log_level;
FILE *log_output;

#define LOG(STATUS, level, format, ...)                                 \
    (((level) <= log_level) ?                                           \
     (fprintf(log_output, #STATUS ": " format "\n", ##__VA_ARGS__), fflush(log_output)) : \
     0)


#define log_section(...)         LOG(SECTION, 12, __VA_ARGS__)
#define log_calloc(amount, size) LOG(CALLOC,  11, "%s:%d: \t%s \t%d * %s = %d * %zd", __FILE__, __LINE__, __func__, amount, #size, amount, (size_t) (size))
#define log_malloc(size)         LOG(MALLOC,  10, "%s:%d: \t%s \t%s = %zd", __FILE__, __LINE__, __func__, #size, (size_t) (size))
#define log_resource(TYPE, size) LOG(RESOURCE, 9, "%s size %d",  #TYPE, size)
#define log_error_in             LOG(ERROR-IN, 8, "%s:%d: %s", __FILE__, __LINE__, __func__)
#define log_info(...)            LOG(INFO,     7, __VA_ARGS__)
#define log_warning(...)         LOG(WARNING,  6, __VA_ARGS__)
#define log_error(...)           LOG(ERROR,    5, __VA_ARGS__)
#define log_user(...)            LOG(USER,     4, __VA_ARGS__)
#define log_late(time)           LOG(LATE,     3, "%d", time)
#define log_assert(v, ve)        LOG(ASSERTION-FAILURE, 2, "%s:%d: %s: \t%s gave %d", __FILE__, __LINE__, __func__, #v, ve)
#define log_fatal(format, ...)  (LOG(FATAL-ERROR, 1, "%s:%d:%s: " format "\nexiting...\n", __FILE__, __LINE__, __func__, ##__VA_ARGS__), \
                                 exit(EXIT_FAILURE))

FILE *log_initialize_file(void);

#define LOG_DISABLE() int log_stored_value ## __FUNC__ = log_level; log_level = 0;
#define LOG_REENABLE() log_level = log_stored_value ## __FUNC__;

#endif
