#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>

#include "debug.h"

#include "initialize.h"
#include "read.h"
#include "list.h"
#include "eval.h"
#include "print.h"
#include "environment.h"
#include "readline.h"
#include "log.h"
#include "string.h"

int main(int argc, char **argv) {
    Environment *environment = initialize();
    List *call_stack = list_create_empty();

    Bool interactive = false;

    Bool finished = false;

    List *statements = list_create_empty();
    while (!finished) {
        Int option = getopt(argc, argv, "ie:l:");
        switch (option) {
        case 'i':
            interactive = true;
            break;
        case 'e':
            {
                Value statement = VALUE_STRING(string_create_from_str(optarg));
                list_push_back(statements, statement);
                break;
            }
        case 'l':
            sscanf(optarg, "%d", &log_level);
            break;
        case -1:
            finished = true;
            break;
        default:
            log_fatal("Illegal option");
            break;
        }
    }

    for (Unt i = 0; i < statements -> length; i++) {
        Value raw = LIST_GET_UNSAFE(statements, i);
        Value statement = read_value(raw);
        /* print(statement); */
        Value result = eval(statement, environment, call_stack);
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
}
