#include <stdlib.h>
#include <SDL2/SDL.h>

#include "environment.h"
#include "worker.h"
#include "debug.h"
#include "list.h"
#include "assert.h"
#include "eval.h"
#include "communication.h"
#include "memory.h"
#include "resource.h"

void worker_update(Environment *environment);

void worker_loop(Environment *environment) {
    while (true) {
        worker_unfreeze = false;
        SDL_SetRenderDrawColor(environment -> renderer,
                               environment -> setting_clear_red,
                               environment -> setting_clear_green,
                               environment -> setting_clear_blue,
                               environment -> setting_clear_alpha);
        SDL_RenderClear(environment -> renderer);

        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                exit(EXIT_SUCCESS);
            }
        }


        if (mutex_trylock(communication_parsed_queue_lock)) {
            if (communication_parsed_queue -> length > 0){
                Value expression = list_pop_front(communication_parsed_queue);
                mutex_unlock(communication_parsed_queue_lock);
                print(expression); printf("\n");
                Value result = eval(expression, environment);
                print(result); printf("\n");
            } else {
                mutex_unlock(communication_parsed_queue_lock);
            }
        }

        worker_update(environment);
        assert(environment -> call_stack -> length == 0);

        SDL_RenderPresent(environment -> renderer);
        memory_update();
        resource_flush_cache();
        SDL_Delay(1);
    }
}

void worker_update(Environment *environment) {
    /* Lookup must be done every frame as the body can be redefined. */
    Value next_update_symbol = environment -> component_next_update;
    /* TODO: or a lambda! */
    if (next_update_symbol.type == SYMBOL) {
        Value function_value;
        Bool found = hash_get(environment -> functions, next_update_symbol, &function_value);
        if (found) {
            Function *update_function = function_value.val.function_val;
            /* Is evaled when the update function is set, not now */
            List *args = environment -> component_next_update_args;
            eval_apply(next_update_symbol, update_function, args, environment);
        } else {
            /* TODO: log error better*/
            log_error("Error when updating, no such function");
        }
    }
}
