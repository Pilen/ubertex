
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_timer.h>
#include "types.h"
#include "graphics.h"
#include "debug.h"
#include "image.h"
#include "assert.h"
#include "string.h"
#include "memory.h"
#include "resource.h"
#include "file.h"

Bool image_get_renderable_from_file(Environment *environment, Value filename, Renderable *target) {
    Image *skeleton = memory_malloc(sizeof(Image));
    skeleton -> path = filename;
    Value result = resource_get(environment, VALUE_IMAGE(skeleton));
    if (result.type == IMAGE) {
        cairo_surface_t *surface = result.val.image_val -> surface;
        target -> data = (void *) surface;
        target -> render = graphics_show_cairo_surface;
        target -> width = cairo_image_surface_get_width(surface);
        target -> height = cairo_image_surface_get_height(surface);
        return true;
    }

    return false;
}

Unt resource_create_image(Environment *environment, Value skeleton) {
    w_assert(skeleton.type == IMAGE);
    Image *image = skeleton.val.image_val;
    w_assert(image -> path.type == STRING);

    image -> refcount = 0;
    image -> created = SDL_GetTicks();
    image -> last_use = image -> created;

    char *filename = image -> path.val.string_val -> text;
    if (strcmp(file_get_extension_str(filename), "png") == 0 ||
        strcmp(file_get_extension_str(filename), "PNG") == 0) {
        // Open pngs with Cairo
        // This is a simple way to ensure correct handling of transparency
        cairo_surface_t *surface = cairo_image_surface_create_from_png(filename);
        if (cairo_surface_status(surface) == CAIRO_STATUS_SUCCESS) {
            Unt width = cairo_image_surface_get_width(surface);
            Unt height = cairo_image_surface_get_height(surface);
            image -> base = NULL;
            image -> surface = surface;
            image -> size = (sizeof(Image) + sizeof(Unt) * width * height); /* Approximate size of image */
            debug("surface %s has a reference count of %d", filename, cairo_surface_get_reference_count(surface));
            return image -> size;
        }
    } // If we cant use cairo, try SDL_image
    SDL_Surface *loaded = IMG_Load(filename);
    if (!loaded) {
        log_error("Unable to find file %s", filename);
        return 0;
    }
    SDL_Surface *base = SDL_CreateRGBSurface(0, loaded -> w, loaded -> h, 32,
                                             0x00FF0000,
                                             0x0000FF00,
                                             0x000000FF,
                                             0xFF000000);
    SDL_BlitSurface(loaded, NULL, base, NULL);
    SDL_FreeSurface(loaded);
    // TODO: Fix missing premultiplication
    // WARNING: COLORS MIGHT NOT BE ACCURATE IF THE SURFACE CONTAINS TRANSPARENT PIXELS!!!
    cairo_surface_t *surface = cairo_image_surface_create_for_data(base -> pixels,
                                                                   CAIRO_FORMAT_ARGB32,
                                                                   base -> w,
                                                                   base -> h,
                                                                   base -> pitch);

    if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
        SDL_FreeSurface(base);
        log_error("Could not convert sdl surface to a cairo surface: %s", cairo_status_to_string(cairo_surface_status(surface)));
    }
    image -> base = base;
    image -> surface = surface;
    image -> size = (sizeof(Image) + sizeof(SDL_Surface) +
                     sizeof(Unt) * base -> w * base -> h); /* Approximate size of image */
    return image -> size;
}
