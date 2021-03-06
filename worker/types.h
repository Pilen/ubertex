#ifndef W_TYPES_H
#define W_TYPES_H

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

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
    CONS,
    VECTOR,
    HASH,
    FUNCTION,
    LAMBDA,
    COMPONENT,
    COLOR,
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
#define VALUE_NIL ((Value) {NIL, {0}})
#define VALUE_SYMBOL(val) ((Value) {SYMBOL, {.symbol_val = val}})
#define VALUE_INTEGER(val) ((Value) {INTEGER, {.integer_val = val}})
#define VALUE_FLOAT(val) ((Value) {FLOAT, {.float_val = val}})
#define VALUE_STRING(val) ((Value) {STRING, {.string_val = val}})
#define VALUE_CONS(val) ((Value) {CONS, {.cons_val = val}})
#define VALUE_VECTOR(val) ((Value) {VECTOR, {.vector_val = val}})
#define VALUE_HASH(val) ((Value) {HASH, {.hash_val = val}})
#define VALUE_FUNCTION(val) ((Value) {FUNCTION, {.function_val = val}})
#define VALUE_LAMBDA(val) ((Value) {LAMBDA, {.lambda_val = val}})
#define VALUE_COMPONENT(val) ((Value) {COMPONENT, {.component_val = val}})
#define VALUE_COLOR(val) ((Value) {COLOR, {.color_val = val}})
#define VALUE_SOUND(val) ((Value) {SOUND, {.sound_val = val}})
#define VALUE_IMAGE(val) ((Value) {IMAGE, {.image_val = val}})
#define VALUE_PDF(val) ((Value) {PDF, {.pdf_val = val}})
#define VALUE_SOUNDSAMPLE(val) ((Value) {SOUNDSAMPLE, {.soundsample_val = val}})
/* #define VALUE_TEXT(val) ((Value) {TEXT, {.text_val = val}}) */

/* Actual datatype declarations */
typedef struct String_s String;
typedef struct Cons_s Cons;
typedef struct Vector_s Vector;
typedef struct Hash_s Hash;
typedef struct Function_s Function;
typedef struct Lambda_s Lambda;
typedef struct Component_s Component;
typedef struct Color_s Color;
typedef struct Sound_s Sound;
typedef struct Image_s Image;
typedef struct Pdf_s Pdf;
typedef struct Soundsample_s Soundsample;
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
        Cons *cons_val;
        Vector *vector_val;
        Hash *hash_val;
        Function *function_val;
        Lambda *lambda_val;
        Component *component_val;
        Color *color_val;
        Sound *sound_val;
        Image *image_val;
        Pdf *pdf_val;
        Soundsample *soundsample_val;
        /* Text *text_val; */
    } val;
} Value;

#endif
