#include "debug.h"

#include "stdio.h"

#include "print.h"
#include "string.h"
#include "list.h"
#include "symbol.h"

void print(Value value) {
    print_on(output, value);
}

void print_on(FILE *stream, Value value) {
    switch (value.type) {
    case ERROR:
        fprintf(stream, "error");
        break;
    case NIL:
        fprintf(stream, "nil");
        break;
    case SYMBOL:
        {
            Value name = symbol_name(value);
            String *string = name.val.string_val;
            fprintf(stream, string -> text);
        }
        break;
    case INTEGER:
        fprintf(stream, "%d", value.val.integer_val);
        break;
    case FLOAT:
        fprintf(stream, "%lf", value.val.float_val);
        break;
    case STRING:
        fprintf(stream, "\"%s\"", value.val.string_val -> text);
        break;
    case LIST:
        fprintf(stream, "(");
        if (value.val.list_val -> length >= 1) {
            print_on(stream, LIST_GET_UNSAFE(value.val.list_val, 0));
        }
        for (Unt i = 1; i < value.val.list_val -> length; i++) {
            fprintf(stream, " ");
            print_on(stream, LIST_GET_UNSAFE(value.val.list_val, i));
        }
        fprintf(stream, ")");
        break;
    case HASH:
        fprintf(stream, "some hash value");
        break;
    default:
        fprintf(stream, "Illegal type: %d", value.type);
        break;
    }
    fflush(stream);
}
