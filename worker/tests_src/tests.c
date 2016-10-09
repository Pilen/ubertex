#include "test_utils.h"
#include "../log.h"
#include "../initialize.h"

DEFINE_TEST_PROGRAM {
    initialize();
    log_level = 7;
    TEST_USE_SUITE(vector);
    TEST_USE_SUITE(hash);
    TEST_USE_SUITE(read);
    TEST_USE_SUITE(memory);
} END_TEST_PROGRAM
