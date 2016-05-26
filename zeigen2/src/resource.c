#include <sys/stat.h>
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
#include "image.h"
#include "pdf.h"
#include "sound.h"
#include "text.h"

Bool resource_create(Environment *environment, Value skeleton);
Int resource_comparison(const void *a, const void *b);


void resource_initialize(void) {
    resource_cache = hash_create();
    resource_cache_lock = lock_rw_create();
    resource_list = list_create_empty();
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

    if (found) {
        /* This can be done here as we know resources are never changed, modified or removed during a frame update (flush is never called now) */
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
            list_push_back(resource_list, resource);
        }
    }
    lock_write_unlock(resource_cache_lock);

    return resource;
}

Bool resource_create(Environment *environment, Value resource) {
    /* lock_write_lock(resource_cache_lock); Should be hold by caller */
    Unt initial_score = 1;

    Bool created;
    Unt size = 0;
    switch (resource.type) {
    case IMAGE:
        created = resource_create_image(environment, resource, initial_score, &size);
        break;
    case PDF:
        created = resource_create_pdf(environment, resource, initial_score, &size);
        break;
    case SOUNDSAMPLE:
        created = resource_create_soundsample(environment, resource, initial_score, &size);
        break;
    case TEXT:
        created = resource_create_text(environment, resource, initial_score, &size);
        break;
    default:
        z_assert(false);
    }
    log_resource(resource.type, size);
    resource_total_size += size;
    return created;
}

Unt resource_destroy(Value resource) {
    Unt size;
    /* z_assert(resource not in resource_list); */
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
    case TEXT:
        size = resource.val.text_val -> size;
        SDL_DestroyTexture(resource.val.text_val -> texture);
        break;
    default:
        /* Catch missing destructors */
        z_assert(false);
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

    /* z_assert(environment -> call_stack -> length == 0); */
    lock_write_lock(resource_cache_lock);

    /* Only flush if enough memory has been consumed */
    if (resource_total_size < resource_size_threshold) {
        lock_write_unlock(resource_cache_lock);
        return 0;
    }

    z_assert(resource_list -> start == 0);
    qsort(resource_list -> data,
          resource_list -> length,
          sizeof(Value),
          resource_comparison);
    Unt cleared = 0;
    while (resource_list -> length > 0 && resource_total_size >= resource_size_threshold) {
        Value resource = list_pop_back(resource_list);
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
    List *old_resource_list = resource_list;
    resource_list = list_create_empty();
    int64_t unixtime = (uint64_t) time(NULL);
    Unt current_time = SDL_GetTicks();
    int64_t started = unixtime - (current_time / 1000);
    while (old_resource_list -> length > 0) {
        struct stat file_stat;
        Value resource = list_pop_front(old_resource_list);
        Int found;
        int64_t modified;
        char *filename;
        switch (resource.type) {
        case IMAGE:
            z_assert(resource.val.image_val -> path.type == STRING);
            filename = resource.val.image_val -> path.val.string_val -> text;
            found = stat(filename, &file_stat);
            if (found != 0) {
                log_error("File is gone");
                /* Keep the current */
                list_push_back(resource_list, resource);
                break;
            }
            modified = file_stat.st_mtime;
            if (modified + OPTION_RESOURCE_MODIFICATION_BLEED > started + resource.val.image_val -> created) {
                /* Resource is dirty */
                /* Dont push into new resource_list */
                hash_delete(resource_cache, resource);
                cleared += resource_destroy(resource);
            } else {
                list_push_back(resource_list, resource);
            }
        default:
            /* Catch missing */
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
    case PDF:
        a_score = av -> val.pdf_val -> score;
        break;
    case SOUNDSAMPLE:
        a_score = av -> val.soundsample_val -> score;
        break;
    case TEXT:
        a_score = av -> val.soundsample_val -> score;
    default:
        z_assert(false);
    }

    switch (bv -> type) {
    case IMAGE:
        b_score = bv -> val.image_val -> score;
        break;
    case PDF:
        b_score = bv -> val.pdf_val -> score;
        break;
    case SOUNDSAMPLE:
        b_score = bv -> val.soundsample_val -> score;
        break;
    case TEXT:
        b_score = bv -> val.soundsample_val -> score;
    default:
        z_assert(false);
    }

    if (a_score == b_score) {
        return 0;
    } else {
        return a_score < b_score ? -1 : 1;
    }
}
