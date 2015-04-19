#ifndef Z_STRING_H
#define Z_STRING_H

#include "ztypes.h"

struct String_s {
    Unt refcount;
    Unt size;
    char *text;
};

String *string_create_from_substr(char* str, Unt length);
String *string_create_from_str(char *str);

#endif
