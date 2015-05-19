#include "test_utils.h"
#include "../log.h"

DEFINE_TEST_PROGRAM {
    log_level = 7;
    TEST_USE_SUITE(list);
    TEST_USE_SUITE(hash);
    TEST_USE_SUITE(read);
} END_TEST_PROGRAM
