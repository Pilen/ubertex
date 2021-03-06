#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <signal.h>
#include <unistd.h>
#include <sys/stat.h>
#include <execinfo.h>

#include "headers.h"

void segfault_handler(int sig) {
    (void) sig;
    const int n = 30;
    void *array[n];
    size_t size;
    size = backtrace(array, n);
    fprintf(stderr, "SEGFAULT: \n");
    backtrace_symbols_fd(array, size, STDERR_FILENO);
    exit(1);
}

int main(int argc, char **argv) {
    signal(SIGSEGV, segfault_handler);
    Environment *environment = initialize();

    Vector *statements = vector_create_empty();

    Bool interactive = false;
    Bool test_only = false;
    int log_level_execution = OPTION_LOG_LEVEL_EXECUTION;
    char *host = NULL;
    Int port = OPTION_DEFAULT_PORT;
    Bool fullscreen = true;
    Bool background = false;

    Bool finished = false;
    while (!finished) {
        Int option = getopt(argc, argv, "bd:e:h:il:p:r:tw");
        switch (option) {
        case 'b': // Background process (like a server/daemon)
            background = true;
            break;
        case 'd': { // Directory to work in
            struct stat dir_stat;
            if (stat(optarg, &dir_stat) == -1) {
                log_info("Created dir: %s", optarg);
                mkdir(optarg, 0777);
            }
            if (chdir(optarg) != 0) {
                log_fatal("Could not enter directory %s", optarg);
            }
            break;
        }
        case 'e': { // Eval expression
            Value statement = VALUE_STRING(string_create_from_str(optarg));
            vector_push_back(statements, statement);
            break;
        }
        case 'h': // Hostname
            host = optarg;
            break;
        case 'i': // Interactive
            interactive = true;
            test_only = true;
            break;
        case 'l': // Log level
            if (strcmp(optarg, "max") == 0) {
                log_level_execution = LOG_LEVEL_MAX;
            } else {
                sscanf(optarg, "%d", &log_level_execution);
            }
            break;
        case 'p': // Port
            sscanf(optarg, "%d", &port);
            break;
        case 'r': // Resource threshold (approximate memory usage)
            sscanf(optarg, "%zud", &resource_size_threshold);
            break;
        case 't': // Test only, don't start the loop
            test_only = true;
            break;
        case 'w': // Windowed mode
            fullscreen = false;
            break;
        case -1:
            finished = true;
            break;
        default:
            log_fatal("Illegal option");
            break;
        }
    }

    log_level = log_level_execution;

    if (background) {
        /* Moving this section to the last before interactive / looping causes the program to crash. */
        /* XIO:  fatal IO error 11 (Resource temporarily unavailable) on X server ":0" */
        /*        after 286 requests (286 known processed) with 15 events remaining. */
        pid_t process_id = fork();
        if (process_id < 0) {
            log_fatal("Could not fork process!");
        }
        if (process_id > 0) {
            /* Parrent process */
            exit(0);
        }
        pid_t sid = setsid();
        if (sid < 0) {
            /* log_fatal("Could not set sid!"); */
        }
        log_output = log_initialize_file();
        output = log_output;
        fflush(log_output);
        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        close(STDERR_FILENO);
    }


    if (!host) {
        host = NEW_BUFFER(char, 256);
        memset(host, '\0', 256);
        snprintf(host, 255, "tcp://*:%d", port);
    }

    if (!test_only) {
        initialize_graphics(fullscreen, environment);
        communication_initialize(port);
     }
    /* Must be done after initialize_graphics as SDL registers its own function */
    /* It is not very pretty that cleanup is not performed, as the atexit functions are not called */
    signal(SIGINT, SIG_DFL);
    /* log_info("sizeof(int) = %lu bytes", sizeof(int)); */

    log_section("====STATEMENT-EXECUTION====");
    for (Unt i = 0; i < statements -> length; i++) {
        Value raw = VECTOR_GET_UNSAFE(statements, i);
        Value statement = read_value(raw);
        log_section("====EVALUATION====");
        Value result = eval(statement, environment);
        log_section("====EVALUATION-END====");
        print(result);
        fprintf(output, "\n");

    }

    if (interactive) {
        while (true) {
            char *line = w_readline("> ");
            if (strcmp(line, "(quit)") == 0 || strcmp(line, "(exit)") == 0) {
                break;
            }
            Value statement = read_from_str(line);
            free(line);
            Value result = eval(statement, environment);
            print(result);
            fprintf(output, "\n");
        }
    }

    if (!test_only) {
        loop(environment);
    }
    fflush(log_output);
    return EXIT_SUCCESS;
}
