#include <stdio.h>

#include "debug.h"

#include "symbol.h"
#include "read.h"
#include "eval.h"
#include "print.h"

int main(int argc, char **argv) {
    symbol_initialize();
    if (argc == 2) {
        Value read_val = read_from_str(argv[1]);
        Value eval_val = eval(read_val);
        print(eval_val);
    }
    printf("\n");
}
