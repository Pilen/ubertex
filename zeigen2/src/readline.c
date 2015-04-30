
#include <stdio.h>
#include "options.h"

#if OPTION_USE_READLINE
#include "readline.h"
#include <readline/rlstdc.h>
#include <readline/readline.h>
#include <readline/history.h>
#endif

char *z_readline(const char *prompt) {
#if OPTION_USE_READLINE
    char *line = readline(prompt);
    if (line && *line) {
        add_history(line);
    }
    return line;
#else
    printf(prompt);
    char *line = NULL;
    size_t n = 0;
    getline(&line, &n, stdin);
    return line;
#endif
}
