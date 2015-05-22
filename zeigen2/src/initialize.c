#include <stdlib.h>
#include <SDL2/SDL.h>

#include "options.h"
#include "debug.h"
#include "initialize.h"
#include "symbol.h"
#include "lisp.h"
#include "environment.h"
#include "log.h"
#include "resource.h"
#include "memory.h"

Environment *initialize(void) {
    /* static Bool initialized = false; */
    /* if (initialized) { */
    /*     return; */
    /* } */
    /* initialized = true; */
    log_level = OPTION_LOG_LEVEL_INITIALIZATION;

    log_section("====INITIALIZE====");
    memory_initialize();
    symbol_initialize();
    Environment *environment = environment_create();
    lisp_initialize(environment);
    resource_initialize();
    log_section("====INITIALIZE-END====")
    return environment;
}

void initialize_SDL(Environment *environment) {
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        log_fatal("Unable to initialize SDL: %s", SDL_GetError());
    }
    atexit(SDL_Quit);

    SDL_Window *window = SDL_CreateWindow(OPTION_PROGRAM_NAME,
                                          0,0,400,400,0
                                          /* SDL_WINDOWPOS_UNDEFINED, */
                                          /* SDL_WINDOWPOS_UNDEFINED, */
                                          /* 0, 0, */
                                          /* SDL_WINDOW_FULLSCREEN_DESKTOP */
                                          );
    if (!window) {
        log_fatal("Unable to create a window: %s", SDL_GetError());
    }
    environment -> window = window;

    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
    if (!renderer) {
        log_fatal("Unable to create a renderer: %s", SDL_GetError());
    }
    environment -> renderer = renderer;


    /* NOTE: Is it wrong to present the window already? */
    SDL_SetRenderDrawColor(environment -> renderer, 0, 0, 0, 255);
    SDL_RenderClear(environment -> renderer);
    SDL_RenderPresent(environment -> renderer);

}
