#include "debug.h"

#include "stdio.h"

#include "zprint.h"
#include "zstring.h"
#include "zsymbol.h"

void print(Value value) {
    switch (value.type) {
    case ERROR:
        printf("error");
        break;
    case NIL:
        printf("nil");
        break;
    case SYMBOL:
        {
            Value name = symbol_name(value);
            debugv("name.type = %d", name.type);
            String *string = name.val.string_val;
            debugv("%s", "\n\n\n");
            debugv("string = %p", (void *) string);
            printf(string -> text);
        }
        break;
    case INTEGER:
        printf("%d", value.val.integer_val);
        break;
    case FLOAT:
        printf("%lf", value.val.float_val);
        break;
    case STRING:
        printf("\"%s\"", value.val.string_val -> text);
        break;
    case LIST:
        printf("(some list)");
        break;
    case HASH:
        printf("some hash value");
        break;
    default:
        printf("Illegal type: %d", value.type);
        break;
    }
}
