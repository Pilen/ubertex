#include <string.h>

#include "debug.h"
#include "string.h"
#include "memory.h"

/* TODO: this file does not work with unicode yet!!! */

/* TODO: no way of escaping charactercodes!!! */

/* Strings are based on incomplete structs
   as described in http://www.informit.com/guides/content.aspx?g=cplusplus&seqNum=288 */

String *string_create_from_substr(char* str, Unt size) {
    /* TODO: As this only works with ASCII now, it holds that length == size.
       This is not true for unicode?!. */
    String *string = memory_cmalloc(sizeof(String) + sizeof(char) * size + 1);
    /* Add 1 for null char when allocating
       Cleared by calloc */
    /* char *text = memory_calloc(length + 1, sizeof(char)); */

    string -> refcount = 0;
    /* string -> length = length; */
    string -> size = size;
    /* string -> text = text; */

    for (Unt i = 0, j = 0; i < size && str[i] != '\0'; i++, j++) {
        if (str[i] == '\\') {
            debug("\\ incomming");
            i++;
            if (i >= size ) {
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

Bool string_compare(String *a, String *b) {
    return strcmp(a -> text, b -> text);
}

Bool string_compare_str(String *a, char* b) {
    return strcmp(a -> text, b);
}
