
#include "types.h"
#include "memory.h"
#include "hash.h"

static Hash *symbol_table;
static Hash *symbol_names;
static Unt symbol_next_id;

void symbol_initialize() {
    symbol_table = hash_create();
    symbol_names = hash_create();
    symbol_next_id = 0;
}

Value symbol_add(Value name) {
    /* TODO: a `hash_in' function might be useful */
    Value symbol = hash_get(symbol_table, name);
    if (symbol.type == NIL) {
        symbol = VALUE_SYMBOL(symbol_next_id);
        hash_set(symbol_table, name, symbol);
        hash_set(symbol_names, symbol, name);
        symbol_next_id++;
    }
    return symbol;
}

Value symbol_name(Value symbol){
    return hash_get(symbol_names, symbol);
}
