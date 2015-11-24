#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../graphics.h"
#include "../string.h"
#include "../pdf.h"

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
    SDL_SetRenderDrawColor(environment -> renderer, 255, 50, 128, 255);
    SDL_RenderClear(environment -> renderer);

    SDL_SetRenderDrawColor(environment -> renderer, 255, 255, 255, 255);
    SDL_RenderDrawLine(environment -> renderer, 1024/2 - 300, 768/2, 1024/2+300, 768/2);
    SDL_RenderDrawLine(environment -> renderer, 1024/2, 768/2 - 300, 1024/2, 768/2 + 300);
    SDL_Texture *text_test2(Environment *environment);
    SDL_Texture *texture = text_test2(environment);
    graphics_render_centered_at(environment, texture, 1024/2, 768/2);
    SDL_RenderPresent(environment -> renderer);
    SDL_Delay(10000000);

    return VALUE_NIL;
}

LISP_BUILTIN(pdf_test, "") {
    /* Pdf pdf; */
    /* pdf.path = VALUE_STRING(string_create_from_str("/tmp/laesehovedet.pdf")); */
    /* pdf_create(environment, VALUE_PDF(&pdf), 1); */
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
