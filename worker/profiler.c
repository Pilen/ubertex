
#include "headers.h"

#if OPTION_PROFILE
Profile *profiler_create(char *name);
void profiler_reset(Profile *profile);

void profiler_initialize(void) {
    profile_total = profiler_create("total");
    profile_render = profiler_create("render");
    profile_present = profiler_create("present");
    profile_cairo = profiler_create("cairo");
    profile_loop = profiler_create("loop");
}

void profiler_total_report(void) {
    Double total = PROFILER_TO_SEC(profile_total);
    Double render = PROFILER_TO_SEC(profile_render);
    Double present = PROFILER_TO_SEC(profile_present);
    Double cairo = PROFILER_TO_SEC(profile_cairo);
    if (total == 0) {
        fprintf(log_output, "total: %f\n", total);
    } else {
        fprintf(log_output, "total: %f (%.0f%%), render: %.0f%%, present: %.0f%%, cairo: %.0f%% %i\n", total, (total / (1.0 / OPTION_FPS)) * 100 , render/total * 100.0, present/total * 100.0, cairo/total * 100.0, profile_cairo -> count);
    }
    profiler_reset(profile_total);
    profiler_reset(profile_render);
    profiler_reset(profile_present);
    profiler_reset(profile_cairo);
}

struct timespec profiler_diff(struct timespec start, struct timespec end);

void profiler_start(Profile *profile) {
    clock_gettime(CLOCK_MONOTONIC, &profile -> current);
}

void profiler_reset(Profile *profile) {
    profile -> time.tv_sec = 0;
    profile -> time.tv_nsec = 0;
    profile -> count = 0;
}

void profiler_end(Profile *profile) {
    struct timespec time;
    clock_gettime(CLOCK_MONOTONIC, &time);
    struct timespec diff = profiler_diff(profile -> current, time);
    profile -> count++;
    profile -> time.tv_sec += diff.tv_sec;
    profile -> time.tv_nsec += diff.tv_nsec;
    while (profile -> time.tv_nsec > 1000000000) {
        profile -> time.tv_sec += 1;
        profile -> time.tv_nsec -= 1000000000;
    }
}

void profiler_report(Profile *profile) {
    if (!(profile -> time.tv_sec == 0 && profile -> time.tv_nsec == 0)) {
        fprintf(log_output, "profile %s %llds %lldns, count %d, %lld.%fns\n",
                profile -> name,
                (long long) profile -> time.tv_sec,
                (long long) profile -> time.tv_nsec,
                profile -> count,
                (long long) profile -> time.tv_sec / profile -> count,
                ((double) profile -> time.tv_nsec / profile -> count)/1000000000.0
                );
    }
    profiler_reset(profile);
}

Profile *profiler_create(char *name) {
    Profile *profile = memory_cmalloc(sizeof(Profile));
    profile -> name = name;
    return profile;
}

struct timespec profiler_diff(struct timespec start, struct timespec end) {
    struct timespec temp;
    if ((end.tv_nsec - start.tv_nsec) < 0) {
        temp.tv_sec = end.tv_sec - start.tv_sec -1;
        temp.tv_nsec = 1000000000 + end.tv_nsec - start.tv_nsec;
    } else {
        temp.tv_sec = end.tv_sec - start.tv_sec;
        temp.tv_nsec = end.tv_nsec - start.tv_nsec;
    }
    return temp;
}

#endif
