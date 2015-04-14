#ifndef Z_LISP_H
#define Z_LISP_H

#include <stdint.h>

/* Type aliases for C types */
typedef int32_t Int;
typedef uint32_t Unt;
typedef float Float;
typedef double Double;


/* Enumeration of lisp types */
typedef enum {
    SYMBOL,
    INTEGER,
    FLOAT,
    STRING,
    LIST,
    HASH,
} Type;


/* Actual datatype declarations */
typedef List;
typedef String;
typedef Hash;

/* Definition of lisp values */
typedef struct {
    Type type;
    union {
        Unt symbol_val;
        Int integer_val;
        double float_val;
        String *string_val;
        List *list_val;
        Hash *hash_val;
    };
} Value;


#endif
