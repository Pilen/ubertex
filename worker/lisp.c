
#include "headers.h"

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
    LISP_REGISTER_BUILTIN_FROM_RAW(setcar, setcar, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(setcdr, setcdr, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(type-of, type_of, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(float, float, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(length, length, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(nth, nth, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(elt, nth, true, environment);

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
    LISP_REGISTER_BUILTIN_FROM_RAW(lexical-let, lexical_let, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(funcall, funcall, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(+, plus, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(-, minus, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(*, times, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(/, division, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(mod, mod, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(>, greater_than, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(abs, abs, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(log, log, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(log2, log2, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(log10, log10, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sin, sin, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(cos, cos, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(radians-to-degrees, rad_to_deg, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(rad2deg, rad_to_deg, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(rad-to-deg, rad_to_deg, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(deg, rad_to_deg, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(degrees-to-radians, deg_to_rad, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(deg2rad, deg_to_rad, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(deg-to-rad, deg_to_rad, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(rad, deg_to_rad, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(pow, pow, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(expt, pow, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sqr, sqr, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sqrt, sqrt, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(floor, floor, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(ceil, ceil, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(round, round, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(randint, randint, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(choice, choice, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(ease, ease, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(frame, frame, true, environment);

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

    LISP_REGISTER_BUILTIN_FROM_RAW(defcomp, define_component, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(defcomponent, define_component, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(create, create_component, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(destroy, destroy_component, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(set-layer, set_layer, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(layer, set_layer, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(current-layer, current_layer, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(deflocal, deflocal, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(update, update, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(background, background, false, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(foreground, foreground, false, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(send, send, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(broadcast, broadcast, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(receive, receive, false, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(assert, assert, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(component_update_all, component_update_all, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(message_dispatch, message_dispatch, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(resource_cache_size, resource_cache_size, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sounds_playing, resource_cache_size, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(allocate_useless, allocate_useless, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(render_test, render_test, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(pdf_test, pdf_test, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(sdl_internals, sdl_internals, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(resource_usage, resource_usage, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(exit_program, exit_program, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(set_window_position, set_window_position, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(force_frame, force_frame, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(inspect, inspect, true, environment);


    LISP_REGISTER_BUILTIN_FROM_RAW(f, f, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(split-lines, split_lines, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(remove-chars, remove_chars, true, environment);
    LISP_REGISTER_BUILTIN_FROM_RAW(count-lines, count_lines, true, environment);

    LISP_REGISTER_BUILTIN_FROM_RAW(read-file, read_file, true, environment);





    /* Variables: */
    hash_set(environment -> global_variables, symbols_t, symbols_t);
}

void lisp_register_builtin(Value symbol, c_lisp_function c_function, Bool eval, String *docstring, Environment *environment) {
    Function *function = NEW(Function);
    function -> eval = eval;
    function -> c_code = true;
    function -> c_function = c_function;
    function -> parameters = VALUE_NIL;
    function -> body = (Value) {ERROR, {0}}; /* Done this way to avoid logging */
    function -> docstring = docstring;
    Value function_value = VALUE_FUNCTION(function);

    hash_set(environment -> functions, symbol, function_value);
}
