#ifndef Z_LISP_H
#define Z_LISP_H

#include <stdint.h>

/* Type aliases for C types */
typedef int32_t Int;
typedef uint32_t Unt;
typedef float Float;
typedef double Double;
typedef Int Bool;

#define true 1;
#define false 0;


/* Enumeration of lisp types */
typedef enum {
    ERROR, /* A special symbol */
    NIL, /* A special symbol */
    SYMBOL,
    INTEGER,
    FLOAT,
    STRING,
    LIST,
    HASH,
} Type;

/* TODO: ensure this is a perfectly legal/good way of defining structs with unions
   If not replace all instances of this pattern in the code
*/
#define VALUE_ERROR (Value){ERROR, {0}}
#define VALUE_NIL (Value){NIL, {0}}
#define VALUE_SYMBOL(val) (Value){SYMBOL, {.symbol_val = val}}
#define VALUE_INTEGER(val) (Value){INTEGER, {.integer_val = val}}
#define VALUE_FLOAT(val) (Value){FLOAT, {.float_val = val}}
#define VALUE_STRING(val) (Value){STRING, {.string_val = val}}
#define VALUE_LIST(val) (Value){LIST, {.list_val = val}}
#define VALUE_HASH(val) (Value){HASH, {.hash_val = val}}

/* Actual datatype declarations */
typedef struct List_s List;
typedef struct String_s String;
typedef struct Hash_s Hash;

/* Definition of lisp values */
typedef struct {
    Type type;
    union {
        Unt symbol_val;
        Int integer_val;
        Double float_val;
        String *string_val;
        List *list_val;
        Hash *hash_val;
    } val;
} Value;

#endif
