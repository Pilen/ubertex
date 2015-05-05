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

/* Symbols for types
   remember that error and nil are not true types, but symbols */
Value symbols_symbol;
Value symbols_integer;
Value symbols_float;
Value symbols_string;
Value symbols_list;
Value symbols_hash;

#endif
