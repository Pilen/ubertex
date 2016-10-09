#include <sys/stat.h>
#include <SDL2/SDL.h>

#include "types.h"
#include "resource.h"
#include "hash.h"
#include "vector.h"
#include "string.h"
#include "environment.h"
#include "assert.h"
#include "lock.h"
#include "memory.h"
#include "debug.h"
#include "image.h"
#include "pdf.h"
#include "sound.h"

Bool resource_create(Environment *environment, Value skeleton);
Int resource_comparison(const void *a, const void *b);


void resource_initialize(void) {
    resource_cache = hash_create();
    resource_cache_lock = lock_rw_create();
    resource_vector = vector_create_empty();
    resource_total_size = 0;
    size_t available = memory_estimate_available();
    resource_size_threshold = available * (OPTION_RESOURCE_PERCENTAGE / 100.0);
}

Value resource_get(Environment *environment, Value skeleton) {
    /* Resource might be eq to skeleton, might not */
    Value resource;

    lock_read_lock(resource_cache_lock);
    /* Hashes are based on their creation data, eg. file path */
    Bool found = hash_get(resource_cache, skeleton, &resource);
    lock_read_unlock(resource_cache_lock);

    Unt new_use = SDL_GetTicks();
    if (found) {
        switch (resource.type) {
        case IMAGE:
            resource.val.image_val -> last_use = new_use;
            break;
        case PDF:
            resource.val.pdf_val -> last_use = new_use;
            break;
        case SOUNDSAMPLE:
            resource.val.soundsample_val -> last_use = new_use;
            break;
        default:
            w_assert(false);
        }
        return resource;
    }

    /* Resource must be created/loaded from disk */
    lock_write_lock(resource_cache_lock);
    /* Ensure the resource has not been created by a simultaneous thread */
    found = hash_get(resource_cache, skeleton, &resource);
    if (!found) {
        found = resource_create(environment, skeleton);
        if (found) {
            resource = skeleton;
            hash_set(resource_cache, skeleton, resource);
            vector_push_back(resource_vector, resource);
        }
    }
    lock_write_unlock(resource_cache_lock);

    return resource;
}

Bool resource_create(Environment *environment, Value resource) {
    /* lock_write_lock(resource_cache_lock); Should be hold by caller */
    Unt size;
    switch (resource.type) {
    case IMAGE:
        size = resource_create_image(environment, resource);
        break;
    case PDF:
        size = resource_create_pdf(environment, resource);
        break;
    case SOUNDSAMPLE:
        size = resource_create_soundsample(environment, resource);
        break;
    default:
        w_assert(false);
    }
    log_resource(resource.type, size);
    resource_total_size += size;
    return size > 0;
}

Unt resource_destroy(Value resource) {
    Unt size;
    /* w_assert(resource not in resource_vector); */
    lock_write_lock(resource_cache_lock);
    hash_delete(resource_cache, resource);
    switch (resource.type) {
    case IMAGE:
        size = resource.val.image_val -> size;
        cairo_surface_destroy(resource.val.image_val -> surface);
        SDL_FreeSurface(resource.val.image_val -> base);
        break;
    case PDF:
        size = resource.val.pdf_val -> size;
        for (Int i = 0; i < resource.val.pdf_val -> pagecount; i++) {
            cairo_surface_destroy(resource.val.pdf_val -> pages[i]);
        }
        break;
    case SOUNDSAMPLE:
        /* Assumes no sounds in sound_table are playing the soundsample */
        size = resource.val.soundsample_val -> size;
        Mix_FreeChunk(resource.val.soundsample_val -> chunk);
        break;
    default:
        /* Catch missing destructors */
        w_assert(false);
    }
    resource_total_size -= size;
    lock_write_unlock(resource_cache_lock);
    return size;
}

Unt resource_shrink_cache(void) {
    /* WARNING: SHOULD ONLY EVER BE CALLED WHILE
       NO REFERENCES TO THE ACTUAL RESOURCES ARE HELD!
       Aka, don't call it during a frame update as the actual resources pointed at could become invalid.
       Else we need some kind of threadsafe metric of this resource being in use right now and probably a lock pr. resource.
    */

    /* w_assert(environment -> call_stack.type == NIL); */
    lock_write_lock(resource_cache_lock);

    /* Only flush if enough memory has been consumed */
    if (resource_total_size < resource_size_threshold) {
        lock_write_unlock(resource_cache_lock);
        return 0;
    }

    vector_normalize(resource_vector);
    qsort(resource_vector -> data,
          resource_vector -> length,
          sizeof(Value),
          resource_comparison);
    Unt cleared = 0;
    while (resource_vector -> length > 0 && resource_total_size >= resource_size_threshold) {
        Value resource = vector_pop_front(resource_vector);
        if (resource.type == SOUNDSAMPLE) {
            if (resource.val.soundsample_val -> current == 0) {
                cleared += resource_destroy(resource);
            } else {
                /* Sound is being played currently */
                /* Size not counted among stuff cleared now */
                sound_mark_dirty(resource.val.soundsample_val -> path);
            }
        } else {
            cleared += resource_destroy(resource);
        }
    }
    lock_write_unlock(resource_cache_lock);

    return cleared;
}

Unt resource_flush_entire_cache(void) {
    size_t old_resource_size_threshold = resource_size_threshold;
    resource_size_threshold = 0;
    Unt cleared = resource_shrink_cache();
    resource_size_threshold = old_resource_size_threshold;
    return cleared;
}

Unt resource_flush_dirty_cache(void) {
    /* Reloading dirty files should not be done here,
     * the resources might not be needed now (or ever again) incurring a bigger cost for this function.
     * Instead a seperate thread might do this.
    */
    debug("Flushing dirty cache");

    lock_write_lock(resource_cache_lock);

    Unt cleared = 0;
    Vector *old_resource_vector = resource_vector;
    resource_vector = vector_create_empty();
    int64_t unixtime = (uint64_t) time(NULL);
    Unt current_time = SDL_GetTicks();
    int64_t started = unixtime - (current_time / 1000);
    while (old_resource_vector -> length > 0) {
        struct stat file_stat;
        Value resource = vector_pop_front(old_resource_vector);
        Int found;
        int64_t modified;
        char *filename;
        switch (resource.type) {
        case IMAGE:
            w_assert(resource.val.image_val -> path.type == STRING);
            filename = resource.val.image_val -> path.val.string_val -> text;
            found = stat(filename, &file_stat);
            if (found != 0) {
                log_error("File is gone");
                /* Keep the current */
                vector_push_back(resource_vector, resource);
                break;
            }
            modified = file_stat.st_mtime;
            if (modified + OPTION_RESOURCE_MODIFICATION_BLEED > started + resource.val.image_val -> created) {
                /* Resource is dirty */
                /* Dont push into new resource_vector */
                hash_delete(resource_cache, resource);
                cleared += resource_destroy(resource);
            } else {
                vector_push_back(resource_vector, resource);
            }
        default:
            /* Catch missing */
            w_assert(false);
        }
    }
    lock_write_unlock(resource_cache_lock);

    return cleared;
}

Int resource_comparison(const void *a, const void *b) {
    Value *av = (Value *) a;
    Value *bv = (Value *) b;
    Unt a_last_use;
    Unt b_last_use;

    switch (av -> type) {
    case IMAGE:
        a_last_use = av -> val.image_val -> last_use;
        break;
    case PDF:
        a_last_use = av -> val.pdf_val -> last_use;
        break;
    case SOUNDSAMPLE:
        a_last_use = av -> val.soundsample_val -> last_use;
        break;
    default:
        w_assert(false);
    }

    switch (bv -> type) {
    case IMAGE:
        b_last_use = bv -> val.image_val -> last_use;
        break;
    case PDF:
        b_last_use = bv -> val.pdf_val -> last_use;
        break;
    case SOUNDSAMPLE:
        b_last_use = bv -> val.soundsample_val -> last_use;
        break;
    default:
        w_assert(false);
    }

    return a_last_use - b_last_use;
}
