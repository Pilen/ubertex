#ifndef Z_TYPES_H
#define Z_TYPES_H

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

#include "log.h"

/* Type aliases for C types */
typedef int32_t Int;
typedef uint32_t Unt;
typedef float Float;
typedef double Double;
typedef bool Bool;

/* #define true 1 */
/* #define false 0 */


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
    FUNCTION,
    SOUND,

    /* Resources: */
    IMAGE,
    PDF,
    SOUNDSAMPLE,
    /* TEXT, */
} Type;

/* TODO: ensure this is a perfectly legal/good way of defining structs with unions
   If not replace all instances of this pattern in the code
*/
/* TODO: ensure an error is logged every time this is called */
#define VALUE_ERROR (log_error_in, (Value) {ERROR, {0}})
/* #define VALUE_ERROR ((Value) {ERROR, {0}}) */
#define VALUE_NIL ((Value) {NIL, {0}})
#define VALUE_SYMBOL(val) ((Value) {SYMBOL, {.symbol_val = val}})
#define VALUE_INTEGER(val) ((Value) {INTEGER, {.integer_val = val}})
#define VALUE_FLOAT(val) ((Value) {FLOAT, {.float_val = val}})
#define VALUE_STRING(val) ((Value) {STRING, {.string_val = val}})
#define VALUE_LIST(val) ((Value) {LIST, {.list_val = val}})
#define VALUE_HASH(val) ((Value) {HASH, {.hash_val = val}})
#define VALUE_FUNCTION(val) ((Value) {FUNCTION, {.function_val = val}})
#define VALUE_IMAGE(val) ((Value) {IMAGE, {.image_val = val}})
#define VALUE_PDF(val) ((Value) {PDF, {.pdf_val = val}})
#define VALUE_SOUNDSAMPLE(val) ((Value) {SOUNDSAMPLE, {.soundsample_val = val}})
#define VALUE_SOUND(val) ((Value) {SOUND, {.sound_val = val}})
/* #define VALUE_TEXT(val) ((Value) {TEXT, {.text_val = val}}) */

#define IS_NUMERIC(val) ((val).type == INTEGER || (val).type == FLOAT)
#define NUM_VAL(v) ((Double) (((v).type == INTEGER) ? (v).val.integer_val : ((v).type == FLOAT) ? (v).val.float_val : NAN))

/* Actual datatype declarations */
typedef struct List_s List;
typedef struct String_s String;
typedef struct Hash_s Hash;
typedef struct Function_s Function;
typedef struct Image_s Image;
typedef struct Pdf_s Pdf;
typedef struct Soundsample_s Soundsample;
typedef struct Sound_s Sound;
/* typedef struct Text_s Text; */
typedef struct Renderable_s Renderable;

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
        Function *function_val;
        Sound *sound_val;
        Image *image_val;
        Pdf *pdf_val;
        Soundsample *soundsample_val;
        /* Text *text_val; */
    } val;
} Value;

#endif
