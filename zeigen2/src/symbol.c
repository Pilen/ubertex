#include "debug.h"
#include "types.h"
#include "memory.h"
#include "hash.h"
#include "symbol.h"
#include "string.h"

static Hash *symbol_table;
static Hash *symbol_names;
static Unt symbol_next_id;

void symbol_initialize() {
    symbol_table = hash_create();
    symbol_names = hash_create();
    symbol_next_id = 0;

    symbols_t = symbol_get(VALUE_STRING(string_create_from_str("t")));
    symbols_progn = symbol_get(VALUE_STRING(string_create_from_str("progn")));
    symbols_quote = symbol_get(VALUE_STRING(string_create_from_str("quote")));
    symbols_lambda = symbol_get(VALUE_STRING(string_create_from_str("lambda")));
    symbols_clojure = symbol_get(VALUE_STRING(string_create_from_str("clojure")));
    symbols_ampersand_optional = symbol_get(VALUE_STRING(string_create_from_str("&optional")));
    symbols_ampersand_rest = symbol_get(VALUE_STRING(string_create_from_str("&rest")));

    symbols_symbol = symbol_get(VALUE_STRING(string_create_from_str("symbol")));
    symbols_integer = symbol_get(VALUE_STRING(string_create_from_str("integer")));
    symbols_float = symbol_get(VALUE_STRING(string_create_from_str("float")));
    symbols_string = symbol_get(VALUE_STRING(string_create_from_str("string")));
    symbols_list = symbol_get(VALUE_STRING(string_create_from_str("list")));
    symbols_hash = symbol_get(VALUE_STRING(string_create_from_str("hash")));
}

Value symbol_get(Value name) {
    /* TODO: a `hash_in' function might be useful */
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
    hash_get(symbol_names, symbol, &name);
    return name;
}
