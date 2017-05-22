#include <stdlib.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>

#include "../types.h"
#include "../assert.h"
#include "../eval.h"
#include "../list.h"
#include "../vector.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../graphics.h"
#include "../string.h"
#include "../pdf.h"
#include "../resource.h"
#include "../sound.h"
#include "../image.h"
#include "../component.h"
#include "../message.h"
/* #include "../libs/cairosdl/cairosdl.h" */

LISP_BUILTIN(assert, "") {
    ENSURE_NOT_EMPTY(args);
    Value first = NEXT(args);
    if (args.type == CONS) {
        Value second = NEXT(args);
        debug_value(first);
        debug_value(second);
        if (!equal(first, second)) {
            log_assert(lisp-assertion, 0);
            exit(EXIT_FAILURE);
        }
    } else {
        if (args.type == NIL || args.type == ERROR) {
            log_assert(lisp-assertion, 0);
            exit(EXIT_FAILURE);
        }
    }
    ENSURE_EMPTY(args);
    return VALUE_NIL;
}

LISP_BUILTIN(resource_cache_size, "") {
    lock_read_lock(resource_cache_lock);
    Unt size = resource_total_size;
    lock_read_unlock(resource_cache_lock);
    return VALUE_INTEGER(size);
}

LISP_BUILTIN(sounds_playing, "") {
    extern Int sound_playing;
    extern Mutex *sound_lock;

    mutex_lock(sound_lock);
    Int playing = sound_playing;
    mutex_unlock(sound_lock);
    return VALUE_INTEGER(playing);
}

LISP_BUILTIN(allocate_useless, "") {
    size_t size = 10000;
    Unt *mem = memory_malloc(sizeof(Unt) * size);
    Unt i;
    for (i = 0; i < size; i++) {
        mem[i] = i;
    }
    return VALUE_INTEGER(i);
}

LISP_BUILTIN(render_test, "") {
/*     /\* SDL_SetRenderDrawColor(environment -> renderer, 255, 50, 128, 255); *\/ */
/*     /\* SDL_RenderClear(environment -> renderer); *\/ */

/*     /\* SDL_SetRenderDrawColor(environment -> renderer, 255, 255, 255, 255); *\/ */
/*     /\* SDL_RenderDrawLine(environment -> renderer, 1024/2 - 300, 768/2, 1024/2+300, 768/2); *\/ */
/*     /\* SDL_RenderDrawLine(environment -> renderer, 1024/2, 768/2 - 300, 1024/2, 768/2 + 300); *\/ */
/*     /\* SDL_Texture *text_test2(Environment *environment); *\/ */
/*     /\* SDL_Texture *texture = text_test2(environment); *\/ */
/*     /\* graphics_render_centered_at(texture, 1024/2, 768/2, environment); *\/ */
/*     /\* graphics_present(environment); *\/ */
/*     /\* SDL_Delay(10000000); *\/ */


/*     cairo_set_source_rgb(environment -> cairo, 1.0, 0.1, 0.0); */
/*     cairo_paint(environment -> cairo); */

/*     /\* SDL_Surface *surface = IMG_Load("/home/pilen/bachelor2/forsvar/test.bmp"); *\/ */
/*     SDL_Surface *orig = IMG_Load("/home/pilen/av/2016/billeder/placeholder.png"); */
/*     if (!orig) log_fatal("orig"); */
/*     SDL_Surface *surface = SDL_CreateRGBSurface(0, orig -> w, orig -> h, 32, */
/*                                                 0x00FF0000, */
/*                                                 0x0000FF00, */
/*                                                 0x000000FF, */
/*                                                 0xFF000000); */
/*     if (!surface) log_fatal("surface"); */
/*     SDL_BlitSurface(orig, NULL, surface, NULL); */

/*     cairo_surface_t *cairo_surface = cairo_image_surface_create_for_data(surface -> pixels, */
/*                                                                          CAIRO_FORMAT_ARGB32, */
/*                                                                          surface -> w, */
/*                                                                          surface -> h, */
/*                                                                          surface -> pitch); */
/*     /\* cairo_surface_t *cairo_surface = cairosdl_surface_create(surface); *\\/ *\/ */
/*     /\* cairo_surface_t *cairo_surface = cairo_image_surface_create_from_png("/home/pilen/av/2016/billeder/placeholder.png"); *\/ */
/*     if (cairo_surface_status(cairo_surface) != CAIRO_STATUS_SUCCESS) log_fatal("cairo_surface: %s", cairo_status_to_string(cairo_surface_status(cairo_surface))); */
/*     /\* cairo_set_source_surface(environment -> cairo, cairo_surface, 0, 0); *\/ */
/*     /\* cairo_paint(environment -> cairo); *\/ */
/*     /\* graphics_present(environment); *\/ */
/*     /\* SDL_Delay(1000*4); *\/ */


/* /\* WHY DOES THIS WORK?!?! *\/ */
/*     SDL_Surface *surface2 = SDL_CreateRGBSurface(0, 600, 600, 32, */
/*                                                 0x00FF0000, */
/*                                                 0x0000FF00, */
/*                                                 0x000000FF, */
/*                                                 0xFF000000); */
/*     cairo_surface_t *cairo_surface2 = cairo_image_surface_create_for_data(surface2 -> pixels, */
/*                                                                           CAIRO_FORMAT_ARGB32, */
/*                                                                           surface2 -> w, */
/*                                                                           surface2 -> h, */
/*                                                                           surface2 -> pitch); */
/*     SDL_Window *window = SDL_CreateWindow("fisk", 0, 0, 600, 600, 0); */
/*     SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC); */
/*     cairo_t *cairo = cairo_create(cairo_surface2); */
/*     cairo_set_line_width(cairo, 9); */
/*     cairo_set_source_rgba(cairo, 1.0, 0.0, 0.0, .5); */
/*     cairo_rectangle(cairo,10, 10, 70, 70); */
/*     cairo_stroke(cairo); */
/*     cairo_surface_flush(cairo_surface2); */
/*     SDL_Texture *texture = SDL_CreateTextureFromSurface(renderer, surface2); */
/*     SDL_SetRenderDrawColor(renderer, 100, 100, 100, 255); */
/*     SDL_RenderClear(renderer); */
/*     SDL_SetRenderDrawColor(renderer, 255, 255, 255, 128); */
/*     SDL_Rect rect = {20, 24, 80, 20}; */
/*     SDL_RenderFillRect(renderer, &rect); */
/*     SDL_Rect rect2 = {30, 5, 10, 60}; */
/*     SDL_SetRenderDrawColor(renderer, 255, 0, 0, 128); */
/*     SDL_RenderFillRect(renderer, &rect2); */


/*     SDL_RenderCopy(renderer, texture, NULL, NULL); */
/*     SDL_RenderPresent(renderer); */
/*     debug("fisk"); */
/*     return VALUE_NIL; */

    /* cairo_surface_t *surface = image_get_surface_from_file(VALUE_STRING(string_create_from_str("/home/pilen/av/2016/billeder/test-square.png")), environment); */
    /* Int width = cairo_image_surface_get_width(surface); */
    /* Int height = cairo_image_surface_get_height(surface); */

    /* Double x = 0; */
    /* Double y = 0; */
    /* Double scale = 2.0; */
    /* Double angle = (2.0*M_PI)/8.0; */
    /* cairo_save(environment -> cairo); */
    /* /\* cairo_translate(environment -> cairo, width/2, height/2); *\/ */
    /* /\* cairo_scale(environment -> cairo, scale, scale); *\/ */
    /* /\* cairo_rotate(environment -> cairo, angle); *\/ */
    /* /\* cairo_translate(environment -> cairo, -width/2, -height/2); *\/ */
    /* /\* /\\* cairo_translate(environment -> cairo, -200, -200); *\\/ *\/ */
    /* cairo_translate(environment -> cairo, 200, 200); */
    /* cairo_set_source_surface(environment -> cairo, surface, x, y); */
    /* cairo_paint(environment -> cairo); */
    /* cairo_restore(environment -> cairo); */
    /* cairo_set_source_surface(environment -> cairo, surface, x, y); */
    /* cairo_paint(environment -> cairo); */
    /* /\* graphics_present(environment); *\/ */
    /* /\* SDL_Delay(1000*2); *\/ */
    /* /\* exit(EXIT_SUCCESS); *\/ */
    return VALUE_NIL;
}


LISP_BUILTIN(pdf_test, "") {
    /* Pdf pdf; */
    /* pdf.path = VALUE_STRING(string_create_from_str("/tmp/laesehovedet.pdf")); */
    /* pdf_create(VALUE_PDF(&pdf), 1, environment); */
    return VALUE_NIL;
}

LISP_BUILTIN(sdl_internals, "") {
    if (!environment -> renderer) {
        log_info("No renderer!");
    }
    if (!environment -> window) {
        log_info("No window");
    }
    Int width;
    Int height;
    SDL_GetWindowSize(environment -> window, &width, &height);
    log_info("Window: %dx%d", width, height);
    return VALUE_NIL;
}

LISP_BUILTIN(resource_usage, "") {
    lock_read_lock(resource_cache_lock);
    log_info("Total resource size: %zu", resource_total_size);

    Int resource_comparison(const void *a, const void *b);
    qsort(resource_vector -> data,
          resource_vector -> length,
          sizeof(Value),
          resource_comparison);

    Unt total_size = 0;
    for (Int i = 0; i < resource_vector -> length; i++) {
        Value resource = VECTOR_GET_UNSAFE(resource_vector, i);
        switch (resource.type) {
        case IMAGE:
            total_size += resource.val.image_val -> size;
            w_assert(resource.val.image_val -> path.type == STRING);
            log_info("Image %s, last_use: %u, size: %u",
                     resource.val.image_val -> path.val.string_val -> text,
                     resource.val.image_val -> last_use,
                     resource.val.image_val -> size);

            break;
        case PDF:
            total_size += resource.val.pdf_val -> size;
            w_assert(resource.val.pdf_val -> path.type == STRING);
            log_info("Pdf %s, last_use: %u, size: %u",
                     resource.val.pdf_val -> path.val.string_val -> text,
                     resource.val.pdf_val -> last_use,
                     resource.val.pdf_val -> size);
            break;
        case SOUNDSAMPLE:
            total_size += resource.val.soundsample_val -> size;
            w_assert(resource.val.soundsample_val -> path.type == STRING);
            log_info("SoundSample %s, last_use: %u, size: %u",
                     resource.val.soundsample_val -> path.val.string_val -> text,
                     resource.val.soundsample_val -> last_use,
                     resource.val.soundsample_val -> size);
            break;
        case ERROR:
        case NIL:
        case SYMBOL:
        case INTEGER:
        case FLOAT:
        case STRING:
        case CONS:
        case VECTOR:
        case HASH:
        case FUNCTION:
        case LAMBDA:
        case COMPONENT:
        case COLOR:
        case SOUND:
            break;
        }
    }
    log_info("Total size: %d", total_size);
    lock_read_unlock(resource_cache_lock);
    return VALUE_NIL;
}

LISP_BUILTIN(component_update_all, "") {
    component_update_all(environment);
    return VALUE_NIL;
}

LISP_BUILTIN(message_dispatch, "") {
    message_dispatch(environment);
    return VALUE_NIL;
}

LISP_BUILTIN(exit_program, "") {
    log_info("exit_program called");
    exit(EXIT_SUCCESS);
    return VALUE_NIL;
}

LISP_BUILTIN(set_window_position, "") {
    ENSURE_NOT_EMPTY(args);
    Value x_value = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value y_value = NEXT(args);
    ENSURE_EMPTY(args);
    if (x_value.type != INTEGER && y_value.type != INTEGER) {
        return VALUE_ERROR;
    }
    SDL_SetWindowPosition(environment -> window, x_value.val.integer_val, y_value.val.integer_val);
    return VALUE_NIL;
}

LISP_BUILTIN(force_frame, "") {
    if (args.type == CONS) {
        Value new = NEXT(args);
        ENSURE_EMPTY(args);
        ENSURE(new.type == INTEGER);
        environment -> frame = new.val.integer_val;
    } else {
        environment -> frame++;
    }
    return VALUE_NIL;
}
