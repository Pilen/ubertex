#include "debug.h"

#include "stdio.h"

#include "print.h"
#include "string.h"
#include "list.h"
#include "symbol.h"

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
            String *string = name.val.string_val;
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
        printf("(");
        if (value.val.list_val -> length >= 1) {
            print(LIST_GET_UNSAFE(value.val.list_val, 0));
        }
        for (Unt i = 1; i < value.val.list_val -> length; i++) {
            printf(" ");
            print(LIST_GET_UNSAFE(value.val.list_val, i));
        }
        printf(")");
        break;
    case HASH:
        printf("some hash value");
        break;
    default:
        printf("Illegal type: %d", value.type);
        break;
    }
    fflush(stdout);
}
