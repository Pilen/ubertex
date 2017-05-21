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
#include "profiler.h"

void loop(Environment *environment) {
    Unt next_tick = SDL_GetTicks();
    Unt fast_runs = 0;

    while (true) {
        profiler_start(profile_total);
        log_section("====LOOP====");
        environment -> frame++;

        if (loop_abort) {
            loop_abort = false;
            environment -> update = VALUE_NIL;
            environment -> background = VALUE_NIL;
            environment -> foreground = VALUE_NIL;
            component_destroy_all(environment);
            sound_stop_all();
        }

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


        Value expression; // Only valid inside if expression
        Unt designated_frame; // Only valid inside if expression
        if (communication_extract(environment -> frame, &expression, &designated_frame)) {
            if (designated_frame < environment -> frame && designated_frame != 0) {
                log_late(environment -> frame - designated_frame);
            }
            loop_blank = false;
            print_on(log_output, expression); fprintf(log_output, "\n");
            Value result = eval(expression, environment);
            print_on(log_output, result); fprintf(log_output, "\n");
        }

        profiler_start(profile_loop);
        lock_read_lock(resource_cache_lock);
        environment -> current_layer = OPTION_DEFAULT_LAYER;
        environment -> current_component = NULL;
        eval(environment -> background, environment);
        eval(environment -> update, environment);
        eval(environment -> foreground, environment);
        w_assert(environment -> call_stack.type == NIL);
        w_assert(environment -> dynamic_variables.type == NIL);
        component_update_all(environment);
        w_assert(environment -> call_stack.type == NIL);
        w_assert(environment -> dynamic_variables.type == NIL);
        message_dispatch(environment);
        w_assert(environment -> call_stack.type == NIL);
        w_assert(environment -> dynamic_variables.type == NIL);
        lock_read_unlock(resource_cache_lock);
        profiler_end(profile_loop);

        if (loop_blank) {
            graphics_clear(environment);
        }

        graphics_present(environment);
        memory_update();

        Unt cleared = 0;
        cleared += resource_shrink_cache();
        if (flush_dirty_cache) {
            flush_dirty_cache = false;
            cleared += resource_flush_dirty_cache();
        }
        if (flush_entire_cache) {
            flush_entire_cache = false;
            cleared += resource_flush_entire_cache();
        }
        if (cleared > 0) {
            lock_read_lock(resource_cache_lock);
            log_info("Flushed %d, %zd still in cache", cleared, resource_total_size);
            lock_read_unlock(resource_cache_lock);
        }
        profiler_end(profile_total);
        profiler_total_report();
        fflush(log_output);
        fflush(output);

        next_tick += environment -> skip_ticks;
        Int sleep_time = next_tick - SDL_GetTicks();
        if (sleep_time >= 0) {
            if (sleep_time < 20) {
                /* debug("sleep: %i", sleep_time); */
            }
            SDL_Delay(sleep_time);
            fast_runs = 0;
            environment -> fast_run = false;
        } else {
            /* debug("LATE!"); */
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
