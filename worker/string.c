#include <string.h>

#include "headers.h"

/* TODO: this file does not work with unicode yet!!! */

/* TODO: no way of escaping charactercodes!!! */

/* Strings are based on incomplete structs
   as described in http://www.informit.com/guides/content.aspx?g=cplusplus&seqNum=288 */

String *string_create_from_substr(char* str, Unt bytes) {
    /**
     * Copy Bytes number of bytes from str.
     * Use without the nullbyte
     */
    /* TODO: As this only works with ASCII now, it holds that length == size.
       This is not true for unicode?!. */
    String *string = memory_malloc(sizeof(String) + sizeof(char) * (bytes + 1));
    /* Add 1 for null char when allocating
       Cleared by calloc */
    /* char *text = memory_malloc(sizeof(char) * (length + 1)); */

    string -> refcount = 0;
    /* string -> length = length; */
    string -> size = bytes + 1;
    /* string -> text = text; */

    for (Unt i = 0, j = 0; i < bytes && str[i] != '\0'; i++, j++) {
        if (str[i] == '\\') {
            i++;
            if (i >= bytes ) {
                /* TODO: log error */
                /* No character is inserted when the final is '\' */
                break;
            }
            switch (str[i]) {
            case 'n': string -> text[j] = '\n'; break;
            case 't': string -> text[j] = '\t'; break;
            case '\\': string -> text[j] = '\\'; break; /* Is this line correct? */
            default:                                    /* Is this line correct? */
                string -> text[j] = '\\';               /* Is this line correct? */
                j++;                                    /* Is this line correct? */
                string -> text[j] = str[i]; break;      /* Is this line correct? */
            }
        } else {                                        /* Is this line correct?, previously next happened unconditionally */
            string -> text[j] = str[i];
        }
    }

    return string;
}

String *string_create_from_str(char *str) {
    return string_create_from_substr(str, strlen(str));
}

String *string_duplicate(String *string) {
    String *new_string = memory_malloc(sizeof(String) + sizeof(char) * string -> size);
    new_string -> refcount = 0;
    new_string -> size = string -> size;
    for (Unt i = 0; i < string -> size; i++) {
        new_string -> text[i] = string -> text[i];
    }
    return new_string;
}

String *string_concatenate(String *a, String *b) {
    /* Only one nullbyte */
    Unt size = a -> size - 1 + b -> size;
    String *string = memory_malloc(sizeof(String) + sizeof(char) * size);
    string -> refcount = 0;
    string -> size = size;
    Unt i = 0;
    for (Unt j = 0; j < a -> size - 1; i++, j++) {
        string -> text[i] = a -> text[j];
    }
    for (Unt k = 0; k < b -> size; i++, k++) {
        string -> text[i] = b -> text[k];
    }
    return string;
}

Int string_compare(String *a, String *b) {
    return strcmp(a -> text, b -> text);
}

Int string_compare_str(String *a, char* b) {
    return strcmp(a -> text, b);
}


Value *string_list_from_value(Value value, Value *result, Value original, Bool repr) {
    (void) repr;
    Value string_error = VALUE_STRING(string_create_from_str("error"));
    Value string_nil = VALUE_STRING(string_create_from_str("nil"));
    Value string_doublequote = VALUE_STRING(string_create_from_str("\""));
    Value string_open_paren = VALUE_STRING(string_create_from_str("("));
    Value string_close_paren = VALUE_STRING(string_create_from_str(")"));
    Value string_space = VALUE_STRING(string_create_from_str(" "));
    Value string_dot = VALUE_STRING(string_create_from_str("."));


    Value string_X = VALUE_STRING(string_create_from_str("X"));

    (void) string_error;
    (void) string_nil;
    (void) string_doublequote;
    (void) string_open_paren;
    (void) string_close_paren;
    (void) string_space;
    (void) string_dot;

    (void) string_X;

    switch (value.type) {
    case ERROR:
        CDR(*result) = CONS1(string_error); result = &CDR(*result);
        break;
    case NIL:
        CDR(*result) = CONS1(string_nil); result = &CDR(*result);
        break;
    case SYMBOL:
        CDR(*result) = CONS1(symbol_name(value)); result = &CDR(*result);
        break;
    case INTEGER: {
        char *buffer = memory_malloc(64+1);
        snprintf(buffer, 64, "%d", value.val.integer_val);
        Value str = VALUE_STRING(string_create_from_str(buffer));
        CDR(*result) = CONS1(str); result = &CDR(*result);
        break;
    }
    case FLOAT: {
        char *buffer = memory_malloc(64+1);
        Int i = snprintf(buffer, 64, "%lf", value.val.float_val);
        i--;
        while (i >= 0 && buffer[i] == '0') {
            i--;
        }
        if (buffer[i] == '.') {
            i++;
        }
        i++;
        buffer[i] = '\0';
        Value str = VALUE_STRING(string_create_from_str(buffer));
        CDR(*result) = CONS1(str); result = &CDR(*result);
        break;
    }
    case STRING: {
        if (repr) {
            CDR(*result) = CONS1(string_doublequote); result = &CDR(*result);
        }
        CDR(*result) = CONS1(value); result = &CDR(*result);
        if (repr) {
            CDR(*result) = CONS1(string_doublequote); result = &CDR(*result);
        }
        break;
    }
    case CONS: {
        CDR(*result) = CONS1(string_open_paren); result = &CDR(*result);
        Bool print_space = false;
        while (true) {
            if (print_space) {
                CDR(*result) = CONS1(string_space); result = &CDR(*result);
            }
            print_space = true;
            result = string_list_from_value(CAR(value), result, original, repr);
            if (CDR(value).type != CONS) {
                break;
            }
            value = CDR(value);
        }
        if (CDR(value).type != NIL) {
            CDR(*result) = CONS1(string_space); result = &CDR(*result);
            CDR(*result) = CONS1(string_dot); result = &CDR(*result);
            CDR(*result) = CONS1(string_space); result = &CDR(*result);
            result = string_list_from_value(CDR(value), result, original, repr);
        }
        CDR(*result) = CONS1(string_close_paren); result = &CDR(*result);
        break;
    }
    default:
        break;
    }
    return result;
}

String *string_from_value(Value value, Bool repr) {
    Value items = CONS1(VALUE_NIL); // sentinel value to ensure CAR works
    string_list_from_value(value, &items, items, repr);
    (void) NEXT(items); // Ignore first value (sentinel)

    return string_flatten(items);
}

String *string_flatten(Value list) {
    Unt total_length = 0;
    Value strings = list;

    while (strings.type == CONS) {
        Value val = NEXT(strings);
        w_assert(val.type == STRING);
        String *str = val.val.string_val;
        total_length += string_length(str);
    }

    strings = list;
    String *string = memory_malloc(sizeof(String) + sizeof(char) * (total_length + 1));
    string -> size = total_length + 1;
    char *buffer = string -> text;
    while (strings.type == CONS) {
        Value val = NEXT(strings);
        String *str = val.val.string_val;
        char *letter = str -> text;
        while (*letter != STRING_END) {
            *buffer = *letter;
            buffer++;
            letter++;
        }
    }
    *buffer = STRING_END;
    return string;
}
