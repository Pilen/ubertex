
#include "string.h"

#include "zstring.h"
#include "zmemory.h"

/* TODO: this file does not work with unicode yet!!! */

/* TODO: no way of escaping charactercodes!!! */


String *string_create_from_substr(char* str, Unt length) {
    /* TODO: As this only works with ASCII now, it holds that length == size.
       This is not true for unicode. */
    String *string = z_malloc(sizeof(String));
    char *text = z_calloc(length, sizeof(char));

    string -> refcount = 0;
    string -> size = length;
    string -> text = text;

    for (Unt i = 0; i < length && str[i] != '\0'; i++) {
        text[i] = str[i];
    }

    return string;
}

String *string_create_from_str(char *str) {
    return string_create_from_substr(str, strlen(str) + 1);
}
