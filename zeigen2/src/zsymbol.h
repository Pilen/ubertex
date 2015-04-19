#ifndef Z_SYMBOL_H
#define Z_SYMBOL_H

#include "ztypes.h"

void symbol_initialize();

Value symbol_add(Value name);
Value symbol_name(Value symbol);


#endif
