#include <stdio.h>

#include "debug.h"

#include "zsymbol.h"
#include "zread.h"
#include "zeval.h"
#include "zprint.h"

int main(int argc, char **argv) {
    symbol_initialize();
    if (argc == 2) {
        /* debug("kanin"); */
        Value read_val = read_from_str(argv[1]);
        Value eval_val = eval(read_val);
        print(eval_val);
    }
    printf("\n");
}
