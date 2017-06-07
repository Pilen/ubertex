
#include "test_utils.h"
#include "../headers.h"

void test_read_1(void) {
    Value value = read_from_str("x");
    TEST(value.type == SYMBOL);
}

void test_read_2(void) {
    Value value = read_from_str("1x");
    TEST(value.type == SYMBOL);
}

void test_read_3(void) {
    Value value = read_from_str("x2");
    TEST(value.type == SYMBOL);
}

void test_read_4(void) {
    Value value = read_from_str("1x2");
    TEST(value.type == SYMBOL);
}

void test_read_5(void) {
    Value value = read_from_str("1x2y3z4");
    TEST(value.type == SYMBOL);
}

void test_read_6(void) {
    Value value = read_from_str(" a b ");
    (void) value;
}


DEFINE_TEST_SUITE(read) {
    initialize();
    TEST_ADD(test_read_1());
    TEST_ADD(test_read_2());
    TEST_ADD(test_read_3());
    TEST_ADD(test_read_4());
    TEST_ADD(test_read_5());
    TEST_ADD(test_read_6());
}
