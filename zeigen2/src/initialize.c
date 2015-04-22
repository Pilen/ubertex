
#include "initialize.h"
#include "symbol.h"

void initialize(void) {
    static Bool initialized = false;
    if (initialized) {
        return;
    }
    initialized = true;
    symbol_initialize();
}
