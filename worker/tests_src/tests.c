#include "test_utils.h"
#include "../headers.h"

DEFINE_TEST_PROGRAM {
    initialize();
    log_level = 7;
    TEST_USE_SUITE(hash);
    TEST_USE_SUITE(list);
    TEST_USE_SUITE(string);
    TEST_USE_SUITE(memory);
    TEST_USE_SUITE(read);
    TEST_USE_SUITE(vector);
    TEST_USE_SUITE(math);
} END_TEST_PROGRAM
