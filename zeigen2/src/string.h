#ifndef Z_STRING_H
#define Z_STRING_H

#include "types.h"

struct String_s {
    Unt refcount;
    Unt size;
    /* Unt length; /\* Length of string in bytes without nullbyte *\/ */
    char text[]; /* Incomplete type */
};

String *string_create_from_substr(char* str, Unt length);
String *string_create_from_str(char *str);

Bool string_compare(String *a, String *b);
Bool string_compare_str(String *a, char *b);
#endif
