#include <stdlib.h>
#include <SDL2/SDL.h>

#include "environment.h"
#include "loop.h"
#include "debug.h"
#include "list.h"
#include "assert.h"
#include "eval.h"
#include "communication.h"
#include "memory.h"
#include "resource.h"
#include "sound.h"
#include "graphics.h"
#include "component.h"
#include "message.h"

void loop_update(Environment *environment, Value update_symbol, Value args);

void loop_loop(Environment *environment) {
    Unt next_tick = SDL_GetTicks();
    Unt fast_runs = 0;

    while (true) {
        log_section("====LOOP====");
        if (loop_abort) {
            environment -> update = VALUE_NIL;
            environment -> render = VALUE_NIL;
            component_destroy_all(environment);
            sound_stop_all();
        }
        loop_abort = false;

        graphics_clear(environment);

        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                exit(EXIT_SUCCESS);
            } else if (event.type == SDL_KEYDOWN) {
                if (event.key.keysym.sym == SDLK_ESCAPE) {
                    exit(EXIT_SUCCESS);
                }
            }
        }


        if (mutex_trylock(communication_parsed_queue_lock)) {
            if (communication_parsed_queue -> length > 0){
                Value expression = vector_pop_front(communication_parsed_queue);
                mutex_unlock(communication_parsed_queue_lock);
                loop_blank = false;
                print_on(log_output, expression); fprintf(log_output, "\n");
                Value result = eval(expression, environment);
                print_on(log_output, result); fprintf(log_output, "\n");
            } else {
                mutex_unlock(communication_parsed_queue_lock);
            }
        }

        lock_read_lock(resource_cache_lock);
        environment -> current_layer = OPTION_DEFAULT_LAYER;
        environment -> current_component = NULL;
        eval(environment -> update, environment);
        eval(environment -> render, environment);
        w_assert(environment -> call_stack.type == NIL);
        w_assert(environment -> dynamic_variables.type == NIL);
        component_update_all(environment);
        w_assert(environment -> call_stack.type == NIL);
        w_assert(environment -> dynamic_variables.type == NIL);
        message_dispatch(environment);
        w_assert(environment -> call_stack.type == NIL);
        w_assert(environment -> dynamic_variables.type == NIL);
        lock_read_unlock(resource_cache_lock);

        if (loop_blank) {
            graphics_clear(environment);
        }

        graphics_present(environment);
        memory_update();

        Unt cleared = 0;
        cleared += resource_shrink_cache();
        if (flush_dirty_cache) {
            cleared += resource_flush_dirty_cache();
            flush_dirty_cache = false;
        }
        if (flush_entire_cache) {
            cleared += resource_flush_entire_cache();
            flush_entire_cache = false;
        }
        if (cleared > 0) {
            lock_read_lock(resource_cache_lock);
            log_info("Flushed %d, %zd still in cache", cleared, resource_total_size);
            lock_read_unlock(resource_cache_lock);
        }
        fflush(log_output);
        fflush(output);

        next_tick += environment -> skip_ticks;
        Int sleep_time = next_tick - SDL_GetTicks();
        if (sleep_time >= 0) {
            SDL_Delay(sleep_time);
            fast_runs = 0;
            environment -> fast_run = false;
        } else {
            /* We are running behind! */
            fast_runs++;
            if (fast_runs > OPTION_MAX_FAST_RUNS) {
                fast_runs = 0;
            }
            environment -> fast_run = fast_runs != 0;
        }
        log_section("====LOOP-END====");
    }
}

void loop_update(Environment *environment, Value update_symbol, Value args) {
    /* Lookup must be done every frame as the body can be redefined. */
    /* TODO: or a lambda! */
    if (update_symbol.type == SYMBOL) {
        Value function_value;
        Bool found = hash_get(environment -> functions, update_symbol, &function_value);
        if (found) {
            Function *update_function = function_value.val.function_val;
            /* Is evaled when the update function is set, not now */
            eval_apply(update_symbol, update_function, args, environment);
        } else {
            /* TODO: log error better*/
            log_error("Error when updating, no such function");
        }
    } else if (update_symbol.type == NIL) {
        /* Do nothing */
    } else {
        log_error("Error when updating, not a function");
    }
}
