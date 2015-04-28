
#include "readline.h"
#include <stdio.h>
#include <readline/rlstdc.h>
#include <readline/readline.h>
#include <readline/history.h>

char *z_readline(const char *prompt) {
    return readline(prompt);
}
