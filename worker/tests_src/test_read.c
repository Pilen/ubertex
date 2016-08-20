
#include "test_utils.h"
#include "../debug.h"
#include "../types.h"
#include "../list.h"
#include "../basic.h"
#include "../read.h"
#include "../initialize.h"

void test_read_1(void) {
    Value value = read_from_str(" a b ");
    (void) value;
}

DEFINE_TEST_SUITE(read) {
    initialize();
    TEST_ADD(test_read_1());
}
