#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../eval.h"
#include "../string.h"
#include "../math.h"
#include "../memory.h"
#include "../debug.h"

#define SD(symbol_name) Value local_symbol_##symbol_name = symbol_get(VALUE_STRING(string_create_from_str(#symbol_name)));
#define SDD(local_name, symbol_name) Value local_symbol_##local_name = symbol_get(VALUE_STRING(string_create_from_str(#symbol_name)));
#define S(symbol_name) local_symbol_##symbol_name

LISP_BUILTIN(ease, "") {
    /* (ease from/var target duration [method]) */
    /* http://upshots.org/actionscript/jsas-understanding-easing */

    /* TODO: make it work with `from` being a symbol, then it should update the value in the symbol */
    ENSURE_NOT_EMPTY(args);
    Value from_raw = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value target = NEXT(args);
    Value duration_raw = NEXT_DEFAULT(args, VALUE_FLOAT(1.0));
    Value method = NEXT_DEFAULT(args, symbols_linear);
    ENSURE_EMPTY(args);
    ENSURE(method.type == SYMBOL);
    ENSURE(IS_NUMERIC(duration_raw));
    ENSURE(IS_NUMERIC(target));
    ENSURE(NUM_VAL(duration_raw) >= 0);
    Unt duration = NUM_VAL(duration_raw) * OPTION_FPS;
    Unt start = environment -> frame;
    Unt end = start + duration;

    Value from;
    if (from_raw.type == SYMBOL) {
        ENSURE(environment_lookup_variable(from_raw, &from, environment));
    } else {
        ENSURE(IS_NUMERIC(from_raw));
        from = from_raw;
    }
    Bool integer_math = target.type == INTEGER && from.type == INTEGER;

    Double change = NUM_VAL(target) - NUM_VAL(from);

    (void) method;

    /* Value method_body; */
    /* switch(method.val.symbol_val) { */
    /* case symbols_linear.val.symbol_val: */

    /*     break */
    /*         } */


    Value lexical = VALUE_NIL;

    SDD(division, /);
    SD(float);
    SD(frame);
    SDD(greater_than, >); //TODO: should be greater_than_equal >=
    SD(if);
    SD(let);
    SDD(minus, -);
    SDD(plus, +);
    SD(ratio);
    SD(round);
    SDD(times, *);

    /* Value condition = CONS(S(greater_than), CONS(CONS1(S(frame)), CONS1(VALUE_INTEGER(end)))); */
    /* Value consequent = target; */
    /* Value position = CONS(S(minus), CONS(CONS1(S(frame)), CONS1(VALUE_INTEGER(start)))); */
    /* Value ratio = CONS(S(division), CONS(position, CONS1(VALUE_FLOAT(duration)))); */
    /* Value multiplication = CONS(S(times), CONS(ratio, CONS1(VALUE_FLOAT(change)))); */
    /* Value rounded = integer_math ? CONS(S(round), CONS1(multiplication)) : multiplication; */
    /* Value alternative = CONS(S(plus), CONS(rounded, CONS1(from))); */
    /* Value body = CONS(S(if), CONS(condition, CONS(consequent, CONS1(alternative)))); */


    Value condition = CONS(S(greater_than), CONS(CONS1(S(frame)), CONS1(VALUE_INTEGER(end))));
    Value consequent = integer_math ? target : CONS(S(float), CONS1(target));
    Value position = CONS(S(minus), CONS(CONS1(S(frame)), CONS1(VALUE_INTEGER(start))));
    Value ratio = CONS(S(division), CONS(position, CONS1(VALUE_FLOAT(duration))));
    Value calculation;
    if (method.val.symbol_val == symbols_linear.val.symbol_val) {
        calculation = CONS(S(plus), CONS(CONS(S(times), CONS(S(ratio), CONS1(VALUE_FLOAT(change)))), CONS1(VALUE_FLOAT(change))));
    } else {
        return VALUE_ERROR;
    }
    Value inner = integer_math ? CONS(S(round), CONS1(calculation)) : calculation;
    Value alternative = CONS(S(let), CONS(CONS1(CONS(S(ratio), CONS1(ratio))), CONS1(inner)));
    Value body = CONS(S(if), CONS(condition, CONS(consequent, CONS1(alternative))));

    debug_value(body);
    /*
     * `(lambda () (if (> (frame) ,end)
     *                 ,target
    */
    Lambda *lambda = memory_malloc(sizeof(Lambda));
    lambda -> parameters = VALUE_NIL;
    lambda -> body = body;
    lambda -> docstring = string_create_from_str("Undocumented function");
    lambda -> lexical = lexical;

    return VALUE_LAMBDA(lambda);
}

LISP_BUILTIN(frame, "") {
    ENSURE_EMPTY(args);
    return VALUE_INTEGER(environment -> frame);
}
