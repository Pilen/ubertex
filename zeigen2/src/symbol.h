#ifndef Z_SYMBOL_H
#define Z_SYMBOL_H

#include "types.h"

void symbol_initialize(void);

Value symbol_get(Value name);
Value symbol_name(Value symbol);

Value symbols_t;
Value symbols_quote;
Value symbols_progn;
Value symbols_lambda;
Value symbols_clojure;
Value symbols_ampersand_optional;
Value symbols_ampersand_rest;

#endif