#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

#include "debug.h"

#include "options.h"
#include "initialize.h"
#include "read.h"
#include "list.h"
#include "eval.h"
#include "print.h"
#include "environment.h"
#include "readline.h"
#include "log.h"
#include "string.h"
#include "program.h"
#include "communication.h"

int main(int argc, char **argv) {
    Environment *environment = initialize();
    List *call_stack = list_create_empty();

    List *statements = list_create_empty();

    Bool interactive = false;
    Bool test_only = false;
    int log_level_execution = OPTION_LOG_LEVEL_EXECUTION;
    char *host = NULL;
    Int port = OPTION_DEFAULT_PORT;

    Bool finished = false;
    while (!finished) {
        Int option = getopt(argc, argv, "e:h:il:p:t");
        switch (option) {
        case 'e':
            {
                Value statement = VALUE_STRING(string_create_from_str(optarg));
                list_push_back(statements, statement);
                break;
            }
        case 'h':
            host = optarg;
            break;
        case 'i':
            interactive = true;
            test_only = true;
            break;
        case 'l':
            if (strcmp(optarg, "max") == 0) {
                log_level_execution = LOG_LEVEL_MAX;
            } else {
                sscanf(optarg, "%d", &log_level_execution);
            }
            break;
        case 'p':
            sscanf(optarg, "%d", &port);
            break;
        case 't':
            test_only = true;
            break;
        case -1:
            finished = true;
            break;
        default:
            log_fatal("Illegal option");
            break;
        }
    }

    if (!host) {
        host = malloc(sizeof(char) * 256);
        memset(host, '\0', 256);
        snprintf(host, 255, "tcp://*:%d", port);
    }

    if (!test_only) {
        initialize_SDL(environment);
    }

    log_section("====STATEMENT-EXECUTION====");
    log_level = log_level_execution;

    for (Unt i = 0; i < statements -> length; i++) {
        Value raw = LIST_GET_UNSAFE(statements, i);
        Value statement = read_value(raw);
        /* print(statement); */
        log_section("====EVALUATION====");
        Value result = eval(statement, environment, call_stack);
        log_section("====EVALUATION-END====");
        print(result);
        printf("\n");

    }

    if (interactive) {
        /* Do stuff */
        while (true) {
            char *line = z_readline("> ");
            if (strcmp(line, "(quit)") == 0 || strcmp(line, "(exit)") == 0) {
                break;
            }
            Value statement = read_from_str(line);
            free(line);
            Value result = eval(statement, environment, call_stack);
            print(result);
            printf("\n");
        }
    }

    if (!test_only) {
        communication_initialize(host);
        program_loop(environment);
    }
    return EXIT_SUCCESS;
}
