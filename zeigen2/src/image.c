
#include "image.h"
#include "assert.h"
#include "string.h"
#include "memory.h"
#include "resource.h"

SDL_Texture *image_get_texture_from_file(Environment *environment, Value filename) {
    Image *skeleton = memory_malloc(sizeof(Image));
    skeleton -> path = filename;
    Value result = resource_get(environment, VALUE_IMAGE(skeleton));
    if (result.type == IMAGE) {
        return result.val.image_val -> texture;
    } else {
        return NULL;
    }
}

Bool image_create(Environment *environment, Value skeleton, Unt initial_score, Unt *size) {
    z_assert(skeleton.type == IMAGE);
    Image *image = skeleton.val.image_val;

    z_assert(image -> path.type == STRING);
    char *filename = image -> path.val.string_val -> text;
    SDL_Surface *surface = SDL_LoadBMP(filename);
    if (!surface) {
        log_error("Unable to find file %s", filename);
        return false;
    }
    SDL_Texture *texture;
    texture = SDL_CreateTextureFromSurface(environment -> renderer,
                                           surface);
    image -> refcount = 0;
    image -> score = initial_score;
    image -> texture = texture;
    SDL_FreeSurface(surface);

    Int width, height;
    SDL_QueryTexture(texture, NULL, NULL, &width, &height);
    image -> size = sizeof(Unt) * width * height; /* Approximate size of texture */
    *size = image -> size;

    return true;
}
