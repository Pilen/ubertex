#include <stdlib.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_mixer.h>
#include <cairo.h>

#include "headers.h"

Environment *initialize(void) {
    /* static Bool initialized = false; */
    /* if (initialized) { */
    /*     return; */
    /* } */
    /* initialized = true; */
    log_level = OPTION_LOG_LEVEL_INITIALIZATION;
    log_output = stderr;
    output = stdout;

    log_section("====INITIALIZE====");
    memory_initialize();
    profiler_initialize();
    symbol_initialize();
    Environment *environment = environment_create();
    lisp_initialize(environment);
    resource_initialize();

    flag_lower(loop_abort);
    flag_lower(loop_blank);
    flag_lower(flush_dirty_cache);
    flag_lower(flush_entire_cache);
    flag_lower(loop_resync);

    random_seed(1);

    environment_set_variable(symbols_pi, VALUE_FLOAT(M_PI), environment);
    environment_set_variable(symbols_tau, VALUE_FLOAT(M_PI * 2.0), environment);
    environment_set_variable(symbols_e, VALUE_FLOAT(M_E), environment);

    log_section("====INITIALIZE-END====");
    return environment;
}

void initialize_graphics(Bool fullscreen, Environment *environment) {
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

    /* SDL_Mixer will load libraries dynamically, Mix_Init can preload libraries */
    /* But in 2.0.2 preloading doesn't work */
    /* flags = MIX_INIT_MP3 | MIX_INIT_OGG; */
    flags = 0;
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


    Int x = SDL_WINDOWPOS_UNDEFINED;
    Int y = SDL_WINDOWPOS_UNDEFINED;
    Int w = 0;
    int h = 0;
    flags = SDL_WINDOW_FULLSCREEN_DESKTOP;

    if (!fullscreen) {
        /* TODO: Get the display as an argument */
        Int index = 0;
        index = SDL_GetNumVideoDisplays() - 1; /* Hack to always use extern screen */
        SDL_DisplayMode display;
        SDL_GetDesktopDisplayMode(index, &display);

        x = 0;
        y = 0;
        w = display.w;
        h = display.h;
        flags = 0;
    }
    SDL_Window *window = SDL_CreateWindow(OPTION_PROGRAM_NAME, x, y, w, h, flags);
    if (!window) {
        log_fatal("Unable to create a window: %s", SDL_GetError());
    }
    environment -> window = window;

    Int width;
    Int height;
    SDL_GetWindowSize(window, &width, &height);
    environment -> width = width;
    environment -> height = height;
    log_info("Window size: %ix%i", width, height);

    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC);
    if (!renderer) {
        log_fatal("Unable to create a renderer: %s", SDL_GetError());
    }
    environment -> renderer = renderer;

    if (fullscreen) {
        SDL_ShowCursor(0);
    } else {
        SDL_SetWindowPosition(window, 0, 0);
    }


    SDL_Texture *texture = SDL_CreateTexture(renderer,
                                             SDL_PIXELFORMAT_ARGB8888,
                                             SDL_TEXTUREACCESS_STREAMING,
                                             width, height);
    environment -> base_texture = texture;
    void *pixels;
    int pitch;
    SDL_LockTexture(environment -> base_texture, NULL, &pixels, &pitch);
    cairo_surface_t *surface = cairo_image_surface_create_for_data(pixels,
                                                                   CAIRO_FORMAT_ARGB32,
                                                                   width, height, pitch);
    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
        log_fatal("Unable to create cairo surface from SDL2 texture");
    }
    environment -> cairo_surface = surface;
    cairo_t *cairo = cairo_create(surface);
    environment -> cairo = cairo;

    graphics_clear(environment);
    graphics_present(environment);
}
