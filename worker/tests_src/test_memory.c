#include <string.h>

#include "test_utils.h"
#include "../headers.h"

void test_memory_1(void) {
    Unt times = 100;
    void *allocs[times];
    size_t size = 1000;
    Unt s = 0;
    for (Unt i = 0; i < times; i++) {
        void *mem = memory_malloc(size);
        memset(mem, i, size);
        allocs[i] = mem;
        s += 1;
    }
    s = 0;
    (void) allocs;
    for (Unt i = 0; i < times; i++) {
        Int *v = (Int *) allocs[i];
        s += *v;
    }
    /* debugi(s); */
    /* debug("%ud", s); */
}

DEFINE_TEST_SUITE(memory) {
    TEST_ADD(test_memory_1());
}
