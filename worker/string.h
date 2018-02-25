#ifndef W_STRING_H
#define W_STRING_H

struct String_s {
    Unt refcount;
    Unt size; /* size in bytes including nullbyte */
    /* Unt length; /\* Length of string in bytes without nullbyte *\/ */
    char text[]; /* Incomplete type */
};

#define STRING_END '\0'

String *string_create_from_substr(char* str, Unt bytes);
String *string_create_from_str(char *str);
String *string_duplicate(String *string);
String *string_concatenate(String *a, String *b);

Int string_compare(String *a, String *b);
Int string_compare_str(String *a, char *b);
#define string_length(s) ((s) -> size - 1)
String *string_from_value(Value value);
String *string_flatten(Value list);

#endif
