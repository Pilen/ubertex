#include <SDL2/SDL.h>

#include "types.h"
#include "resource.h"
#include "hash.h"
#include "list.h"
#include "string.h"
#include "environment.h"
#include "assert.h"
#include "lock.h"
#include "memory.h"
#include "debug.h"

Value resource_create(Environment *environment, Value skeleton);
Value resource_get(Environment *environment, Value skeleton);
Int resource_comparison(const void *a, const void *b);


Hash *resource_cache;
Lock_RW *resource_cache_lock;
List *resource_scores; /* The resource_cache_lock must be held while using this */
size_t resource_total_size;

void resource_initialize(void) {
    resource_cache = hash_create();
    resource_cache_lock = lock_rw_create();
    resource_scores = list_create_empty();
    resource_total_size = 0;
    size_t available = memory_estimate_available();
    resource_size_threshold = available / 100 * OPTION_RESOURCE_PERCENTAGE;
}

SDL_Texture *resource_image(Environment *environment, Value filename) {
    Image *skeleton = memory_malloc(sizeof(Image));
    skeleton -> path = filename;
    Value result = resource_get(environment, VALUE_IMAGE(skeleton));
    if (result.type == IMAGE) {
        return result.val.image_val -> texture;
    } else {
        return NULL;
    }
}

Value resource_get(Environment *environment, Value skeleton) {
    Value resource;

    lock_read_lock(resource_cache_lock);
    Bool found = hash_get(resource_cache, skeleton, &resource);
    lock_read_unlock(resource_cache_lock);

    if (found) {
        /* This can be done here as we know resources are never changed, modified or removed during a frame update (flush is never called now) */
        return resource;
    }

    /* Resource must be created/loaded from disk */
    lock_write_lock(resource_cache_lock);
    /* Ensure the resource has not been created by a simultaneous thread */
    found = hash_get(resource_cache, skeleton, &resource);
    if (!found) {
        resource = resource_create(environment, skeleton);
        if (resource.type != ERROR) {
            hash_set(resource_cache, skeleton, resource);
            list_push_back(resource_scores, resource);
        }
    }
    lock_write_unlock(resource_cache_lock);

    return resource;
}

Value resource_create(Environment *environment, Value skeleton) {
    debug("hej");
    Unt initial_score = 1;

    switch (skeleton.type) {
    case IMAGE: {
        Image *image = skeleton.val.image_val;
        z_assert(image -> path.type == STRING);
        char *filename = image -> path.val.string_val -> text;
        SDL_Surface *surface = SDL_LoadBMP(filename);
        if (!surface) {
            SDL_FreeSurface(surface);
            log_error("Unable to find file %s", filename);
            return VALUE_ERROR;
        } else {
            SDL_Texture *texture;
            texture = SDL_CreateTextureFromSurface(environment -> renderer,
                                                   surface);
            image -> texture = texture;
            image -> refcount = 0;
            image -> score = initial_score;
            SDL_FreeSurface(surface);

            Int w, h;
            SDL_QueryTexture(texture, NULL, NULL, &w, &h);
            image -> size = sizeof(Int) * w * h; /* Approximate size of texture */
            resource_total_size += image -> size;

            return VALUE_IMAGE(image);
        }
    }
    default:
        z_assert(false);
    }
    return VALUE_ERROR;
}

Unt resource_flush_cache(void) {
    /* WARNING: SHOULD ONLY EVER BE CALLED WHILE
       NO REFERENCES TO THE ACTUAL RESOURCES ARE HELD!
       Aka, don't call it during a frame update as the actual resources pointed at could become invalid.
       Else we need some kind of threadsafe metric of this resource being in use right now and probably a lock pr. resource.
    */

    /* z_assert(environment -> call_stack -> length == 0); */
    lock_write_lock(resource_cache_lock);

    /* Only flush if enough memory has been consumed */
    if (resource_total_size < resource_size_threshold) {
        lock_write_unlock(resource_cache_lock);
        return 0;
    }

    z_assert(resource_scores -> start == 0);
    qsort(resource_scores -> data,
          resource_scores -> length,
          sizeof(Value),
          resource_comparison);
    Unt cleared = 0;
    while (resource_scores -> length > 0 && resource_total_size >= resource_size_threshold) {
        Value value = list_pop_back(resource_scores);
        hash_delete(resource_cache, value);
        switch (value.type) {
        case IMAGE:
            resource_total_size -= value.val.image_val -> size;
            SDL_DestroyTexture(value.val.image_val -> texture);
            break;
        default:
            z_assert(false);
        }
    }
    lock_write_unlock(resource_cache_lock);

    return cleared;

}

Int resource_comparison(const void *a, const void *b) {
    Value *av = (Value *) a;
    Value *bv = (Value *) b;
    Unt a_score;
    Unt b_score;

    switch (av -> type) {
    case IMAGE:
        a_score = av -> val.image_val -> score;
        break;
    default:
        z_assert(false);
    }

    switch (bv -> type) {
    case IMAGE:
        b_score = bv -> val.image_val -> score;
        break;
    default:
        z_assert(false);
    }

    if (a_score == b_score) {
        return 0;
    } else {
        return a_score < b_score ? -1 : 1;
    }
}