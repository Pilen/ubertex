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

Resource *resource_create(Environment *environment, Resource_type type, void *resource_data);
Int resource_comparison(const void *a, const void *b);

List *resource_scores;
Mutex *resource_scores_lock; /* Must be aquired while holding the other resource locks to avoid deadlocks */

Hash *resource_images;
Lock_RW *resource_images_lock;

void resource_initialize(void) {
    resource_scores = list_create_empty();
    resource_scores_lock = mutex_create();
    resource_images = hash_create();
    resource_images_lock = lock_rw_create();
}

SDL_Texture *resource_get_image(Environment *environment, Value filename) {
    SDL_Texture *texture = NULL;
    Value resource_v;
    Resource *resource;

    assert(filename.type == STRING);

    lock_read_lock(resource_images_lock);
    Bool found = hash_get(resource_images, filename, &resource_v);
    lock_read_unlock(resource_images_lock);

    if (found) {
        /* This can be done here as we know resources are never changed or modified during a frame update (flush is never called now) */
        resource = resource_v.val.resource_val;
        assert(resource -> type == RESOURCE_TEXTURE);
        texture = resource -> val.texture_val;
        return texture;
    }

    /* Texture must be loaded from disk */
    /* Ensure the texture has not been loaded while by a simultaneous thread */
    lock_write_lock(resource_images_lock);
    found = hash_get(resource_images, filename, &resource_v);
    if (!found) {
        char *filename_str = filename.val.string_val -> text;
        SDL_Surface *surface = SDL_LoadBMP(filename_str);
        if (!surface) {
            log_error("Unable to find file %s", filename_str);
            texture = NULL;
        } else {
            texture = SDL_CreateTextureFromSurface(environment -> renderer, surface);
            resource = resource_create(environment, RESOURCE_TEXTURE, texture);
            resource_v = VALUE_RESOURCE(resource);
            hash_set(resource_images, filename, resource_v);

            mutex_lock(resource_scores_lock);
            list_push_back(resource_scores, resource_v);
            mutex_unlock(resource_scores_lock);
        }
        SDL_FreeSurface(surface);
    }
    lock_write_unlock(resource_images_lock);

    return texture;
}

Resource *resource_create(Environment *environment, Resource_type type, void *resource_data) {
    Unt initial_score = 1;

    Resource *resource = memory_malloc(sizeof(Resource));
    resource -> refcount = 0;
    resource -> score = initial_score;
    resource -> type = type;

    switch (type) {
    case RESOURCE_TEXTURE:
        resource -> val.texture_val = (SDL_Texture *) resource_data;
        break;
    default:
        assert(false);
    }
    return resource;
}

Unt resource_flush_cache(Environment *environment, Unt amount) {
    /* WARNING: SHOULD ONLY EVER BE CALLED WHILE
       NO REFERENCES TO THE ACTUAL RESOURCES ARE HELD!
       Aka, don't call it during a frame update as the actual resources pointed at could become invalid.
       Else we need some kind of threadsafe metric of this resource being in use right now and probably a lock pr. resource.
    */

    assert(environment -> call_stack -> length == 0);
    lock_write_lock(resource_images_lock);
    mutex_lock(resource_scores_lock); /* Must be locked last to avoid deadlocks */
    assert(resource_scores -> start == 0);
    qsort(resource_scores -> data,
          resource_scores -> length,
          sizeof(Value),
          resource_comparison);
    Unt cleared = 0;
    while (resource_scores -> length > 0 && cleared < amount) {
        list_pop_back(resource_scores);
    }
    mutex_unlock(resource_scores_lock);
    lock_write_unlock(resource_images_lock);

    return cleared;

}

Int resource_comparison(const void *a, const void *b) {
    Value *av = (Value *) a;
    Value *bv = (Value *) b;

    assert(av -> type == RESOURCE);
    assert(bv -> type == RESOURCE);

    Unt a_score = av -> val.resource_val -> score;
    Unt b_score = bv -> val.resource_val -> score;
    if (a_score == b_score) {
        return 0;
    } else {
        return a_score < b_score ? -1 : 1;
    }
}
