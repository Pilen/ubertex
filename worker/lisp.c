#include "debug.h"

#include "lisp.h"
#include "symbol.h"
#include "memory.h"
#include "string.h"


void lisp_initialize(Environment *environment) {
    LISP_REGISTER_BUILTIN(symbols_progn, progn, false, environment);
    LISP_REGISTER_BUILTIN(symbols_quote, quote, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(eval, eval, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(list, list, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(cons, cons, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(car, car, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(hd, car, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(head, car, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(cdr, cdr, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(tl, cdr, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(tail, cdr, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(if, if, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(when, when, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(unless, unless, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(while, while, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(and, and, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(or, or, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(print, print, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(set, set, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(setq, setq, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(let, let, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(let*, let_star, false, environment);


    LISP_REGISTER_BUILTIN_FROM_RAW(eq, eq, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(equal, equal, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(not, not, true, environment);



    LISP_REGISTER_BUILTIN_FROM_RAW(defun, defun, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(lambda, lambda, false, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(+, plus, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(-, minus, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(*, times, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(>, greater_than, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sin, sin, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(cos, cos, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(randint, randint, true, environment);


    LISP_REGISTER_BUILTIN_FROM_RAW(color, color, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(clear-color, clear_color, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(clear, clear, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(fill, fill, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(image, image, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(pdf, pdf, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(text, text, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(calibrate, calibrate, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(sound, sound, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sound-stop, sound_stop, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sound-stop-all, sound_stop_all, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sound-fade-all, sound_fade_all, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(next-update, next_update, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(next-post, next_post, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(defcomp, define_component, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(defcomponent, define_component, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(create, create_component, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(destroy, destroy_component, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(current-layer, current_layer, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(deflocal, deflocal, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(update, update, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(render, render, false, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(send, send, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(broadcast, broadcast, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(receive, receive, false, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(component_update_all, component_update_all, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(message_dispatch, message_dispatch, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(resource_cache_size, resource_cache_size, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sounds_playing, resource_cache_size, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(allocate_useless, allocate_useless, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(render_test, render_test, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(pdf_test, pdf_test, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sdl_internals, sdl_internals, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(resource_usage, resource_usage, true, environment);





    /* Variables: */
    hash_set(environment -> global_variables, symbols_t, symbols_t);
}

void lisp_register_builtin(Value symbol, c_lisp_function c_function, Bool eval, String *docstring, Environment *environment) {
    Function *function = memory_malloc(sizeof(Function));
    function -> eval = eval;
    function -> c_code = true;
    function -> c_function = c_function;
    function -> parameters = VALUE_NIL;
    function -> body = (Value) {ERROR, {0}}; /* Done this way to avoid logging */
    function -> docstring = docstring;
    Value function_value = VALUE_FUNCTION(function);

    hash_set(environment -> functions, symbol, function_value);
}
