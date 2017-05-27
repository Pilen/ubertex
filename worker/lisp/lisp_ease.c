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
#define T(m) (method.val.symbol_val == symbols_##m.val.symbol_val)
LISP_BUILTIN(ease, "") {
    /* (ease from/var target duration [method]) */
    /*
     * Visualisation: http://easings.net/
     * Article: http://upshots.org/actionscript/jsas-understanding-easing
     * Original: https://github.com/danro/jquery-easing/blob/master/jquery.easing.js
     * Blog about improving: https://joshondesign.com/2013/03/01/improvedEasingEquations
     * Code with improved: https://gist.github.com/gre/1650294
     */


    /*
     * ;; from target duration
     * ;; start = (frame)
     * ;; end = start + duration
     * ;; change = target - from
     *
     * `(lambda () (if (> (frame) ,end)
     *                 ,target
     *               (let ((ratio (/ (- (frame)
     *                                   start)
     *                               ,duration)))
     *                 (round? (+ (* ratio
     *                                ,change)
     *                             from)))))
     *
     *
     *
     * def ease(start, fro, target, duration):
     *     end = start + duration
     *     change = target - fro
     *     def helper(f):
     *         if f > end:
     *             return target
     *         else:
     *             ratio = ((f - start) / (duration + 0.0))
     *             return ((ratio * change) + fro)
     *     return helper
     *
     */

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
    SD(pow);
    SD(quote);
    SD(ratio);
    SD(round);
    SD(set);
    SDD(times, *);

    Value ease;
    if (T(linear)) {
        ease = S(ratio);
    } else if (T(in_quad)) {
        ease = CONS(S(pow), CONS(S(ratio), CONS1(VALUE_FLOAT(2.0))));
    } else if (T(out_quad)) {
        ease = CONS(S(times), CONS(S(ratio), CONS1(CONS(S(minus), CONS(VALUE_FLOAT(2.0), CONS1(S(ratio)))))));
    } else if (T(in_out_quad)) {
        Value cond = CONS(S(greater_than), CONS(VALUE_FLOAT(0.5), CONS1(S(ratio))));
        Value smaller = CONS(S(times), CONS(VALUE_FLOAT(2.0), CONS(S(ratio), CONS1(S(ratio)))));
        Value minus2 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(2.0))));
        Value minus1 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(1.0))));
        Value larger = CONS(S(times), CONS(VALUE_FLOAT(-2.0), CONS(minus2, CONS1(minus1))));
        ease = CONS(S(if), CONS(cond, CONS(smaller, CONS1(larger))));

    } else if (T(in_cubic)) {
        ease = CONS(S(pow), CONS(S(ratio), CONS1(VALUE_FLOAT(3.0))));
    } else if (T(out_cubic)) {
        Value minus1 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(1.0))));
        ease = CONS(S(plus), CONS(VALUE_FLOAT(1.0), CONS1(CONS(S(pow), CONS(minus1, CONS1(VALUE_FLOAT(3)))))));
    } else if (T(in_out_cubic)) {
        Value cond = CONS(S(greater_than), CONS(VALUE_FLOAT(0.5), CONS1(S(ratio))));
        Value smaller = CONS(S(times), CONS(VALUE_FLOAT(4.0), CONS(S(ratio), CONS(S(ratio), CONS1(S(ratio))))));
        Value minus1 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(1.0))));
        Value minus2twice = CONS(S(times), CONS(VALUE_FLOAT(2.0), CONS1(CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(2.0)))))));
        Value larger = CONS(S(plus), CONS(VALUE_FLOAT(1.0), CONS1(CONS(S(times), CONS(minus1, CONS(minus2twice, CONS1(minus2twice)))))));
        ease = CONS(S(if), CONS(cond, CONS(smaller, CONS1(larger))));

    } else if (T(in_quart)) {
        ease = CONS(S(pow), CONS(S(ratio), CONS1(VALUE_FLOAT(4.0))));
    } else if (T(out_quart)) {
        Value minus1 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(1.0))));
        ease = CONS(S(minus), CONS(VALUE_FLOAT(1.0), CONS1(CONS(S(pow), CONS(minus1, CONS1(VALUE_FLOAT(4.0)))))));
    } else if (T(in_out_quart)) {
        Value cond = CONS(S(greater_than), CONS(VALUE_FLOAT(0.5), CONS1(S(ratio))));
        Value smaller = CONS(S(times), CONS(VALUE_FLOAT(8.0), CONS(S(ratio), CONS(S(ratio), CONS(S(ratio), CONS1(S(ratio)))))));
        Value minus1 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(1.0))));
        Value pow = CONS(S(pow), CONS(minus1, CONS1(VALUE_FLOAT(4.0))));
        Value larger = CONS(S(minus), CONS(VALUE_FLOAT(1.0), CONS1(CONS(S(times), CONS(VALUE_FLOAT(8.0), CONS1(pow))))));
        ease = CONS(S(if), CONS(cond, CONS(smaller, CONS1(larger))));

    } else if (T(in_quint)) {
        ease = CONS(S(pow), CONS(S(ratio), CONS1(VALUE_FLOAT(5.0))));
    } else if (T(out_quint)) {
        Value minus1 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(1.0))));
        ease = CONS(S(plus), CONS(VALUE_FLOAT(1.0), CONS1(CONS(S(pow), CONS(minus1, CONS1(VALUE_FLOAT(5.0)))))));
    } else if (T(in_out_quint)) {
        Value cond = CONS(S(greater_than), CONS(VALUE_FLOAT(0.5), CONS1(S(ratio))));
        Value smaller = CONS(S(times), CONS(VALUE_FLOAT(16.0), CONS(S(ratio), CONS(S(ratio), CONS(S(ratio), CONS(S(ratio), CONS1(S(ratio))))))));
        Value minus1 = CONS(S(minus), CONS(S(ratio), CONS1(VALUE_FLOAT(1.0))));
        Value pow = CONS(S(pow), CONS(minus1, CONS1(VALUE_FLOAT(5.0))));
        Value larger = CONS(S(plus), CONS(VALUE_FLOAT(1.0), CONS1(CONS(S(times), CONS(VALUE_FLOAT(16.0), CONS1(pow))))));
        ease = CONS(S(if), CONS(cond, CONS(smaller, CONS1(larger))));

    } else {
        return VALUE_ERROR;
    }
    Value condition = CONS(S(greater_than), CONS(CONS1(S(frame)), CONS1(VALUE_INTEGER(end))));
    Value consequent = integer_math ? target : CONS(S(float), CONS1(target));
    Value position = CONS(S(minus), CONS(CONS1(S(frame)), CONS1(VALUE_INTEGER(start))));
    Value ratio = CONS(S(division), CONS(position, CONS1(VALUE_FLOAT(duration))));
    Value calculation = CONS(S(plus), CONS(CONS(S(times), CONS(ease, CONS1(VALUE_FLOAT(change)))), CONS1(from)));
    Value inner = integer_math ? CONS(S(round), CONS1(calculation)) : calculation;
    Value let = CONS(S(let), CONS(CONS1(CONS(S(ratio), CONS1(ratio))), CONS1(inner)));
    Value alternative = from_raw.type == SYMBOL ? CONS(S(set), CONS(CONS(S(quote), CONS1(from_raw)), CONS1(let))) : let;
    Value body = CONS(S(if), CONS(condition, CONS(consequent, CONS1(alternative))));

    debug_value(body);

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
