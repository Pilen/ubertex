#include "debug.h"

#include "stdio.h"

#include "print.h"
#include "string.h"
#include "list.h"
#include "vector.h"
#include "hash.h"
#include "symbol.h"
#include "component.h"

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
        {
            /* So there is no way for fprintf to skip trailing zeroes, but always keep atleast one decimal */
            char buffer[64];
            Int i = sprintf(buffer, "%lf", value.val.float_val);
            i--;
            while (i >= 0 && buffer[i] == '0') {
                i--;
            }
            if (buffer[i] == '.') {
                i++;
            }
            i++;
            buffer[i] = '\0';
            fprintf(stream, "%s", buffer);
        }
        break;
    case STRING:
        fprintf(stream, "\"%s\"", value.val.string_val -> text);
        break;
    case CONS:
        {
            fprintf(stream, "(");
            Value list = value;
            while (true) {
                Value element = NEXT(list);
                print_on(stream, element);
                if (list.type == CONS) {
                    fprintf(stream, " ");
                    continue;
                } else if (list.type == NIL) {
                    break;
                } else {
                    fprintf(stream, " . ");
                    print_on(stream, CDR(list));
                    break;
                }
            }
            fprintf(stream, ")");
            break;
        }
    case VECTOR:
        fprintf(stream, "[");
        if (value.val.vector_val -> length >= 1) {
            print_on(stream, VECTOR_GET_UNSAFE(value.val.vector_val, 0));
        }
        for (Unt i = 1; i < value.val.vector_val -> length; i++) {
            fprintf(stream, " ");
            print_on(stream, VECTOR_GET_UNSAFE(value.val.vector_val, i));
        }
        fprintf(stream, "]");
        break;
    case HASH:
        {
            fprintf(stream, "{");
            Hash *table = value.val.hash_val;
            Hash_entry *entry = table -> entries;
            Bool first = true;
            for (Unt i = 0; i < table -> size; i++) {
                if (entry[i].status == HASH_OCCUPIED) {
                    if (first) {
                        first = false;
                    } else {
                        fprintf(stream, ", ");
                    }
                    print_on(stream, entry[i].key);
                    fprintf(stream, ": ");
                    print_on(stream, entry[i].data);
                }
            }
            fprintf(stream, "}");
            break;
        }
    case FUNCTION:
        fprintf(stream, "?");
        break;
    case COMPONENT:
        fprintf(stream, "#<component ");
        print_on(stream, symbol_name(value.val.component_val -> name));
        fprintf(stream, ">");
        break;
    case COLOR:
    case SOUND:
    case IMAGE:
    case PDF:
    case SOUNDSAMPLE:
        fprintf(stream, "Illegal type: %d", value.type);
        break;
    }
    fflush(stream);
}


/* String to_String(Value value) { */
/*     Value value; */
/*     switch (value.type) { */
/*     case ERROR: */
/*         return symbols_error; */
/*     case NIL:s */
/*         return symbols_nil; */
/*     case SYMBOL: */
/*         value = symbol_name(value); */
/*         w_assert(value.type == STRING); */
/*         return value.val.string_val; */
/*     case INTEGER: */
/*         Int number = value.val.integer_val; */
/*         Unt length = ((Unt) floor(log10(number))) + 1 + (number < 0 ? 1 : 0); */
/*         char *buffer[length + 1]; */

/*     case HASH: */
/*     default: */
/*         w_assert(false) */
/*     } */

/* } */
