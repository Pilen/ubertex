#include <stdlib.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_mixer.h>

#include "options.h"
#include "debug.h"
#include "initialize.h"
#include "symbol.h"
#include "lisp.h"
#include "environment.h"
#include "log.h"
#include "resource.h"
#include "memory.h"
#include "assert.h"
#include "sound.h"

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
    Int flags;
    Int result;
    flags = SDL_INIT_VIDEO | SDL_INIT_AUDIO;
    result = SDL_Init(flags);
    if (result != 0) {
        log_fatal("Unable to initialize SDL: %s", SDL_GetError());
    }
    atexit(SDL_Quit);

    flags = IMG_INIT_JPG | IMG_INIT_PNG;
    if (IMG_Init(flags) != flags) {
        log_fatal("Unable to initialize SDL_image: %s", IMG_GetError());
    }
    atexit(IMG_Quit);

    flags = MIX_INIT_MP3 | MIX_INIT_OGG;
    if (Mix_Init(flags) != flags) {
        log_fatal("Unable to initialize SDL_mixer: %s", Mix_GetError());
    }
    atexit(Mix_Quit);
    sound_initialize();

    result = Mix_OpenAudio(OPTION_SOUND_FREQUENCY, OPTION_SOUND_FORMAT,
                           OPTION_SOUND_OUTPUT_CHANNELS, OPTION_SOUND_CHUNKSIZE);
    if (result != 0) {
        log_fatal("Unable to initialize sound: %s", Mix_GetError());
    }
    atexit(Mix_CloseAudio);


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
