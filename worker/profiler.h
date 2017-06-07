#ifndef W_PROFILER_H
#define W_PROFILER_H

#include <time.h>

#define PROFILER_TO_SEC(profile) ((profile) -> time.tv_sec + ((profile) -> time.tv_nsec / 1000000000.0))

typedef struct {
    struct timespec time;
    struct timespec current;
    Unt count;
    char *name;
} Profile;

#if OPTION_PROFILE
void profiler_initialize(void);
Profile *profile_total;
Profile *profile_render;
Profile *profile_present;
Profile *profile_cairo;
Profile *profile_loop;

void profiler_start(Profile *profile);
void profiler_end(Profile *profile);
void profiler_report(Profile *profile);
void profiler_total_report(void);
#else
#define profiler_initialize()
#define profiler_start(profile)
#define profiler_end(profile)
#define profiler_report(profile)
#define profiler_total_report()
#endif


#endif
