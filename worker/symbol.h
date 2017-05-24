#ifndef W_SYMBOL_H
#define W_SYMBOL_H

#include "types.h"

void symbol_initialize(void);

Value symbol_get(Value name);
Value symbol_name(Value symbol);
Value symbol_unique(void);

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
Value symbols_cons;
Value symbols_vector;
Value symbols_hash;

Value symbols_pi;
Value symbols_tau;
Value symbols_e;

Value symbols_plain;
Value symbols_full;
Value symbols_centered;
Value symbols_scaled;
Value symbols_sized;
Value symbols_rotated;

Value symbols_center;
Value symbols_centered;
Value symbols_left;
Value symbols_right;

Value symbols_linear;
Value symbols_in_quad;
Value symbols_out_quad;
Value symbols_in_out_quad;
Value symbols_in_cubic;
Value symbols_out_cubic;
Value symbols_in_out_cubic;
Value symbols_in_quart;
Value symbols_out_quart;
Value symbols_in_out_quart;
Value symbols_in_quint;
Value symbols_out_quint;
Value symbols_in_out_quint;

/* Not used for anything other than printing */
String *symbols_error;
String *symbols_nil;


Value symbols_unreachable;

#endif
