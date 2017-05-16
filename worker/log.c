/* #include <sys/types.h> */
#include <unistd.h>
#include <sys/stat.h>
#include <limits.h>
/* #include <stdio.h> */
#include <string.h>
#include <time.h>

#include "types.h"
#include "log.h"

FILE *log_initialize_file(void) {
    /* Initializes a log file in the installation log dir, and creates a symlink to it in the working dir */

    /* path max defined in linux/limits.h to 4096 */
    Unt my_path_max = PATH_MAX + 28 + 1; // (length "/log/YYYY-MM-DD-hh:mm:ss.log") = 28
    char link[my_path_max];
    char path[my_path_max];

    /* Find executing directory */
    memset(path, 0, sizeof(path)); // readlink does not null terminate!
    pid_t pid = getpid();
    sprintf(link, "/proc/%d/exe", pid);
    if (readlink(link, path, my_path_max) == -1) {
        log_fatal("readlink: No such file or directory: %s\n", link);
    }
    char *p = strrchr(path, '/');

    /* Ensure log directory exist */
    sprintf(p, "/log/");
    struct stat st;
    if (stat(path, &st) == -1) {
        log_info("Log directory missing, creating");
        if (mkdir(path, 0700) == -1) {
            log_fatal("Could not create log directory");
        }

    }

    /* Create log filename */
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    sprintf(p, "/log/%d-%d-%d-%d:%d:%d.log",
           tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday,
           tm.tm_hour, tm.tm_min, tm.tm_sec);

    /* Create log file */
    FILE *log = fopen(path, "w");
    if (!log) {
        log_fatal("Could not open logfile: %s\n", path);
    }
    fprintf(log, "hej\n");

    /* Create symlink */
    if (symlink(path, "log.txt") == -1) {
        log_fatal("Could not create symlink for log.txt");
    }
    return log;
}
