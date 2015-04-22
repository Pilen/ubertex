#include <string.h>

#include "debug.h"
#include "string.h"
#include "memory.h"

/* TODO: this file does not work with unicode yet!!! */

/* TODO: no way of escaping charactercodes!!! */


String *string_create_from_substr(char* str, Unt length) {
    /* TODO: As this only works with ASCII now, it holds that length == size.
       This is not true for unicode?!. */
    String *string = z_malloc(sizeof(String));
    /* Add 1 for null char when allocating
       Cleared by calloc */
    char *text = z_calloc(length + 1, sizeof(char));

    string -> refcount = 0;
    string -> length = length;
    string -> text = text;

    for (Unt i = 0, j = 0; i < length && str[i] != '\0'; i++, j++) {
        if (str[i] == '\\') {
            debug("\\ incomming");
            i++;
            if (i >= length ) {
                /* TODO: log error */
                /* No character is inserted when the final is '\' */
                break;
            }
            debug("switch");
            switch (str[i]) {
            case 'n': text[j] = '\n'; break;
            case 't': text[j] = '\t'; break;
            }
            break;
        }
        text[j] = str[i];
    }

    return string;
}

String *string_create_from_str(char *str) {
    return string_create_from_substr(str, strlen(str));
}
