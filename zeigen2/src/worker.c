#include <stdlib.h>
#include <SDL2/SDL.h>

#include "environment.h"
#include "worker.h"
#include "debug.h"
#include "list.h"
#include "assert.h"
#include "eval.h"
#include "communication.h"

void worker_update(Environment *environment, List *call_stack);

void worker_loop(Environment *environment) {
    SDL_SetRenderDrawColor(environment -> renderer, 255, 0, 0, 255);

    SDL_RenderClear(environment -> renderer);
    SDL_RenderPresent(environment -> renderer);

    /* TODO: place this correctly */
    environment -> component_next_update = VALUE_NIL;
    environment -> component_next_update_args = list_create_empty();

    List *call_stack = list_create_empty();

    while (true) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                exit(EXIT_SUCCESS);
            }
        }

        if (SDL_TryLockMutex(communication_parsed_queue_lock) == 0) {
            if (communication_parsed_queue -> length > 0){
                Value expression = list_pop_front(communication_parsed_queue);
                SDL_UnlockMutex(communication_parsed_queue_lock);
                print(expression);
                printf("\n");
                Value result = eval(expression, environment, call_stack);
                print(result);
                printf("\n");
            } else {
                SDL_UnlockMutex(communication_parsed_queue_lock);
            }
        }

        worker_update(environment, call_stack);
        assert(call_stack -> length == 0);

        SDL_RenderPresent(environment -> renderer);
        SDL_Delay(1000);
    }
}

void worker_update(Environment *environment, List *call_stack) {
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
            eval_apply(next_update_symbol, update_function, args, environment, call_stack);
        } else {
            /* TODO: log error better*/
            log_error("Error when updating, no such function");
        }
    }
}
