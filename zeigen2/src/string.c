#include <string.h>

#include "debug.h"
#include "string.h"
#include "memory.h"

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
    String *string = memory_cmalloc(sizeof(String) + sizeof(char) * (bytes + 1));
    /* Add 1 for null char when allocating
       Cleared by calloc */
    /* char *text = memory_cmalloc(sizeof(char) * (length + 1)); */

    string -> refcount = 0;
    /* string -> length = length; */
    string -> size = bytes + 1;
    /* string -> text = text; */

    for (Unt i = 0, j = 0; i < bytes && str[i] != '\0'; i++, j++) {
        if (str[i] == '\\') {
            debug("\\ incomming");
            i++;
            if (i >= bytes ) {
                /* TODO: log error */
                /* No character is inserted when the final is '\' */
                break;
            }
            switch (str[i]) {
            case 'n': string -> text[j] = '\n'; break;
            case 't': string -> text[j] = '\t'; break;
            }
            break;
        }
        string -> text[j] = str[i];
    }

    return string;
}

String *string_create_from_str(char *str) {
    return string_create_from_substr(str, strlen(str));
}

String *string_duplicate(String *string) {
    String *new_string = memory_cmalloc(sizeof(String) + sizeof(char) * string -> size);
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
    String *string = memory_cmalloc(sizeof(String) + sizeof(char) * size);
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
