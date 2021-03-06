
#include "headers.h"

static Hash *symbol_table;
static Hash *symbol_names;
static Unt symbol_next_id;

void symbol_initialize() {
    symbol_table = hash_create();
    symbol_names = hash_create();
    symbol_next_id = 0;

    symbols_t = symbol_get(VALUE_STRING(string_create_from_str("t")));
    symbols_quote = symbol_get(VALUE_STRING(string_create_from_str("quote")));
    symbols_progn = symbol_get(VALUE_STRING(string_create_from_str("progn")));
    symbols_lambda = symbol_get(VALUE_STRING(string_create_from_str("lambda")));
    symbols_clojure = symbol_get(VALUE_STRING(string_create_from_str("clojure")));
    symbols_ampersand_optional = symbol_get(VALUE_STRING(string_create_from_str("&optional")));
    symbols_ampersand_rest = symbol_get(VALUE_STRING(string_create_from_str("&rest")));

    /* Symbols for types
       remember that error and nil are not true types, but symbols */
    symbols_symbol = symbol_get(VALUE_STRING(string_create_from_str("symbol")));
    symbols_integer = symbol_get(VALUE_STRING(string_create_from_str("integer")));
    symbols_float = symbol_get(VALUE_STRING(string_create_from_str("float")));
    symbols_string = symbol_get(VALUE_STRING(string_create_from_str("string")));
    symbols_cons = symbol_get(VALUE_STRING(string_create_from_str("cons")));
    symbols_vector = symbol_get(VALUE_STRING(string_create_from_str("vector")));
    symbols_hash = symbol_get(VALUE_STRING(string_create_from_str("hash")));

    symbols_pi = symbol_get(VALUE_STRING(string_create_from_str("pi")));
    symbols_tau = symbol_get(VALUE_STRING(string_create_from_str("tau")));
    symbols_e = symbol_get(VALUE_STRING(string_create_from_str("e")));

    symbols_plain = symbol_get(VALUE_STRING(string_create_from_str("plain")));
    symbols_full = symbol_get(VALUE_STRING(string_create_from_str("full")));
    symbols_centered = symbol_get(VALUE_STRING(string_create_from_str("centered")));
    symbols_scaled = symbol_get(VALUE_STRING(string_create_from_str("scaled")));
    symbols_sized = symbol_get(VALUE_STRING(string_create_from_str("sized")));
    symbols_rotated = symbol_get(VALUE_STRING(string_create_from_str("rotated")));

    symbols_center = symbol_get(VALUE_STRING(string_create_from_str("center")));
    symbols_centered = symbol_get(VALUE_STRING(string_create_from_str("centered")));
    symbols_left = symbol_get(VALUE_STRING(string_create_from_str("left")));
    symbols_right = symbol_get(VALUE_STRING(string_create_from_str("right")));

    symbols_linear = symbol_get(VALUE_STRING(string_create_from_str("linear")));
    symbols_in_quad = symbol_get(VALUE_STRING(string_create_from_str("in-quad")));
    symbols_out_quad = symbol_get(VALUE_STRING(string_create_from_str("out-quad")));
    symbols_in_out_quad = symbol_get(VALUE_STRING(string_create_from_str("in-out-quad")));
    symbols_in_cubic = symbol_get(VALUE_STRING(string_create_from_str("in-cubic")));
    symbols_out_cubic = symbol_get(VALUE_STRING(string_create_from_str("out-cubic")));
    symbols_in_out_cubic = symbol_get(VALUE_STRING(string_create_from_str("in-out-cubic")));
    symbols_in_quart = symbol_get(VALUE_STRING(string_create_from_str("in-quart")));
    symbols_out_quart = symbol_get(VALUE_STRING(string_create_from_str("out-quart")));
    symbols_in_out_quart = symbol_get(VALUE_STRING(string_create_from_str("in-out-quart")));
    symbols_in_quint = symbol_get(VALUE_STRING(string_create_from_str("in-quint")));
    symbols_out_quint = symbol_get(VALUE_STRING(string_create_from_str("out-quint")));
    symbols_in_out_quint = symbol_get(VALUE_STRING(string_create_from_str("in-out-quint")));

    symbols_error = string_create_from_str("error");
    symbols_nil = string_create_from_str("nil");

    symbols_unreachable = VALUE_SYMBOL(symbol_next_id++);
    hash_set(symbol_names, symbols_unreachable, VALUE_STRING(string_create_from_str("#<symbol unreachable>")));
}

Value symbol_get(Value name) {
    Value symbol;
    Bool found = hash_get(symbol_table, name, &symbol);
    if (!found) {
        symbol = VALUE_SYMBOL(symbol_next_id);
        hash_set(symbol_table, name, symbol);
        hash_set(symbol_names, symbol, name);
        symbol_next_id++;
    }
    return symbol;
}

Value symbol_name(Value symbol){
    Value name;
    Bool found = hash_get(symbol_names, symbol, &name);
    w_assert(found);
    return name;
}

Value symbol_unique(void) {
    Value symbol = VALUE_SYMBOL(symbol_next_id);
    hash_set(symbol_names, symbol, VALUE_STRING(string_create_from_str("#<symbol unique>")));
    symbol_next_id++;
    return symbol;
}
