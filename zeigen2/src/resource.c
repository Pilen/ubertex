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

List *resource_scores;
Mutex *resource_scores_lock; /* Must be acquired while holding the other resource locks to avoid deadlocks */

Hash *resource_images;
Lock_RW *resource_images_lock;

void resource_initialize(void) {
    resource_scores = list_create_empty();
    resource_scores_lock = mutex_create();
    resource_images = hash_create();
    resource_images_lock = lock_rw_create();
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

    lock_read_lock(resource_images_lock);
    Bool found = hash_get(resource_images, skeleton, &resource);
    lock_read_unlock(resource_images_lock);

    if (found) {
        /* This can be done here as we know resources are never changed, modified or removed during a frame update (flush is never called now) */
        return resource;
    }

    /* Resource must be created/loaded from disk */
    lock_write_lock(resource_images_lock);
    /* Ensure the resource has not been created by a simultaneous thread */
    found = hash_get(resource_images, skeleton, &resource);
    if (!found) {
        resource = resource_create(environment, skeleton);
        if (resource.type != ERROR) {
            hash_set(resource_images, skeleton, resource);
            mutex_lock(resource_scores_lock);
            list_push_back(resource_scores, resource);
            mutex_unlock(resource_scores_lock);
        }
    }
    lock_write_unlock(resource_images_lock);

    return resource;
}

Value resource_create(Environment *environment, Value skeleton) {
    debug("hej");
    Unt initial_score = 1;

    switch (skeleton.type) {
    case IMAGE: {
        Image *image = skeleton.val.image_val;
        assert(image -> path.type == STRING);
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
            return VALUE_IMAGE(image);
        }
    }
    default:
        assert(false);
    }
    return VALUE_ERROR;
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
    /* Value *av = (Value *) a; */
    /* Value *bv = (Value *) b; */

    /* assert(av -> type == IMAGE); */
    /* assert(bv -> type == IMAGE); */

    /* Unt a_score = av -> val.resource_val -> score; */
    /* Unt b_score = bv -> val.resource_val -> score; */
    /* if (a_score == b_score) { */
    /*     return 0; */
    /* } else { */
    /*     return a_score < b_score ? -1 : 1; */
    /* } */
    return -1;
}
