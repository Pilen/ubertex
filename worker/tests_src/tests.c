#include "test_utils.h"
#include "../log.h"
#include "../initialize.h"

DEFINE_TEST_PROGRAM {
    initialize();
    log_level = 7;
    TEST_USE_SUITE(hash);
    TEST_USE_SUITE(list);
    TEST_USE_SUITE(memory);
    TEST_USE_SUITE(read);
    TEST_USE_SUITE(vector);
} END_TEST_PROGRAM
