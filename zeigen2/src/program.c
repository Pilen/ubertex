#include <stdlib.h>
#include <SDL2/SDL.h>

#include "environment.h"
#include "program.h"
#include "debug.h"
#include "list.h"
#include "assert.h"
#include "eval.h"

void program_update(Environment *environment, List *call_stack);

void program_loop(Environment *environment) {
    SDL_SetRenderDrawColor(environment -> renderer, 255, 0, 0, 255);

    debugi(SDL_RenderClear(environment -> renderer));
    SDL_RenderPresent(environment -> renderer);
    debug("hej");

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


        program_update(environment, call_stack);
        assert(call_stack -> length == 0);

        SDL_RenderPresent(environment -> renderer);
        SDL_Delay(1000);
    }
}

void program_update(Environment *environment, List *call_stack) {
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
