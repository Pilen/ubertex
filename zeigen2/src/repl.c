#include <stdio.h>

#include "debug.h"

#include "symbol.h"
#include "read.h"
#include "list.h"
#include "eval.h"
#include "print.h"

int main(int argc, char **argv) {
    symbol_initialize();
    Environment environment = environment_create();
    List *call_stack = list_create_empty();
    if (argc == 2) {
        Value read_val = read_from_str(argv[1]);
        print(read_val);
        printf("\n");
        Value eval_val = eval(read_val, &environment, call_stack);
        print(eval_val);
    }
    printf("\n");
}
