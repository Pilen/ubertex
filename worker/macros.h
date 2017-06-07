#ifndef W_MACROS_H
#define W_MACROS_H

#define IS_NUMERIC(val) ((val).type == INTEGER || (val).type == FLOAT)
#define NUM_VAL(v) ((Double) (((v).type == INTEGER) ? (v).val.integer_val : (((v).type == FLOAT) ? (v).val.float_val : NAN)))

#define IS_LIST(val) ((val).type == CONS || (val).type == NIL)
/* User must ensure the val contains a Cons, unsafe otherwise: */
#define CAR(value) (((value).val.cons_val) -> car)
#define CDR(value) (((value).val.cons_val) -> cdr)
#define CONS(a, b) (VALUE_CONS(list_cons((a), (b))))
#define CONS1(a) (VALUE_CONS(list_cons((a), VALUE_NIL)))

/* Dont[1] rely on the returned value if list is not a CONS cell.
   [1] At least you cant distinguish where the error came from.
   This macro is NOT a statement, nor an expression, use it carefully. */
#define NEXT(list) ((list).type == CONS ? (CAR((list))) : VALUE_ERROR); {if ((list).type == CONS) {(list) = CDR((list));}};
#define NEXT_OR(list, alternative) ((list).type == CONS ? (CAR((list))) : (alternative)); {if ((list).type == CONS) {(list) = CDR((list));}};
#define NEXT_DEFAULT(list, alternative) (((list).type == CONS) ? ((CAR((list)).type == NIL) ? (alternative) : CAR((list))) : (alternative)); {if ((list).type == CONS) {(list) = CDR((list));}};
/* #define IS_EMPTY(list) ((list).type == NIL) */

#define ENSURE_NOT_EMPTY(list) do {if ((list).type != CONS) {return VALUE_ERROR;}} while (0)
#define ENSURE_EMPTY(list) do {if ((list).type != NIL) {return VALUE_ERROR;}} while (0)
#define ENSURE(test) do {if (!(test)) {return VALUE_ERROR;}} while (0)
#define DEFAULT_TO(value, alternative) (((value).type == NIL || (value).type == ERROR) ? alternative : (value))

#endif
