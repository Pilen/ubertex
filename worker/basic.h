#ifndef W_BASIC_H
#define W_BASIC_H

#include "types.h"

Value copy_deep(Value value);
Bool equal(Value a, Value b);
Bool eq(Value a, Value b);

/* Value NILIFY(Value value); */
#define NILIFY(value)                                                   \
    (((value).type == LIST) && ((value).val.list_val -> length == 0) ? VALUE_NIL : (value))
#endif
