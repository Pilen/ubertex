
#include <stdlib.h>
#include <string.h>

#include "headers.h"

void memory_register_collection_hook(void);
void memory_on_garbage_collection(void *obj, void *cd);

Unt memory_garbage_collected;
Mutex *memory_lock;

void memory_initialize(void) {
    GC_INIT();
    GC_allow_register_threads();
    memory_lock = mutex_create();
    memory_garbage_collected = 0;

    memory_register_collection_hook();
}

void *memory_malloc_actual(size_t size) {
    void *allocated = GC_MALLOC(size); // Boehm gc clears the memory
    /* void *allocated = malloc(size); */
    if (!allocated) {
        log_fatal("Internal failure in %s\nUnable to allocate additional memory! %zd bytes requested", __func__, size);
    }
    return allocated;
}

void memory_free(void *ptr) {
    /* This function currently does nothing, the GC will clean it up later */
    (void) ptr;
    /* free(ptr); */
    /* GC_FREE(ptr); */
}

void memory_ref_inc(Value value) {
    (void) value;
    /* log_warning("Reference counting not implemented") */
}
void memory_ref_dec(Value value) {
    (void) value;
    /* log_warning("Reference counting not implemented") */
}

void memory_register_collection_hook(void) {
    /* This is a slight hack to detect when garbagecollection is being run.
       Basically a small chunk of memory is allocated and then ignored to make it garbage.
       A finalizer is called when the memory is collected so we know when it happens */
    /* To avoid allocating an entire page for just a single byte or word we allocate
       a chunk of memory we are fairly sure will be of same size as at least some other allocation. */
    void *control = memory_malloc_actual(sizeof(Unt) * 4);
    GC_REGISTER_FINALIZER(control, memory_on_garbage_collection, NULL, NULL, NULL);
    control = NULL; /* Control is now garbage */

}

void memory_on_garbage_collection(void *obj, void *cd) {
    (void) obj;
    (void) cd;
    memory_register_collection_hook();

    mutex_lock(memory_lock);
    memory_garbage_collected += 1;
    mutex_unlock(memory_lock);
}

void memory_update(void) {
    mutex_lock(memory_lock);
    if (memory_garbage_collected) {
    }
    memory_garbage_collected = 0;
    mutex_unlock(memory_lock);
}

void memory_register_thread(void) {
    struct GC_stack_base local_stack;
    GC_get_stack_base(&local_stack);
    GC_register_my_thread(&local_stack);
}

#ifdef WINDOWS
#include <windows.h>
#else
#include <unistd.h>
#endif
size_t memory_estimate_available(void) {
#ifdef WINDOWS
    MEMORYSTATUSEX status;
    status.dwLength = sizeof(status);
    GlobalMemoryStatusEx(&status);
    return status.ullTotalPhys;
#else
    long pages = sysconf(_SC_PHYS_PAGES);
    long page_size = sysconf(_SC_PAGE_SIZE);
    return pages * page_size;
#endif
}
