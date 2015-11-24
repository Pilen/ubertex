#ifndef Z_PRINTER_H
#define Z_PRINTER_H

#include <stdio.h>
#include "types.h"

FILE *output;

void print(Value value);
void print_on(FILE *stream, Value value);

#endif
