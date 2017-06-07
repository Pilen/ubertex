#include "test_utils.h"
#include "../headers.h"

void test_round_up_to_power_of_2_powers(void) {
    TEST_EQ(2, round_up_to_power_of_2(2));
    TEST_EQ(4, round_up_to_power_of_2(4));
    TEST_EQ(8, round_up_to_power_of_2(8));
    TEST_EQ(16, round_up_to_power_of_2(16));
}

void test_round_up_to_power_of_2_zero(void) {
    TEST_EQ(0, round_up_to_power_of_2(0));
}

void test_round_up_to_power_of_2_all(void) {
    TEST_EQ(0, round_up_to_power_of_2(0));
    TEST_EQ(1, round_up_to_power_of_2(1));
    TEST_EQ(2, round_up_to_power_of_2(2));
    TEST_EQ(4, round_up_to_power_of_2(3));
    TEST_EQ(4, round_up_to_power_of_2(4));
    TEST_EQ(8, round_up_to_power_of_2(5));
    TEST_EQ(8, round_up_to_power_of_2(6));
    TEST_EQ(8, round_up_to_power_of_2(7));
    TEST_EQ(8, round_up_to_power_of_2(8));
    TEST_EQ(16, round_up_to_power_of_2(9));
    TEST_EQ(16, round_up_to_power_of_2(10));
    TEST_EQ(16, round_up_to_power_of_2(11));
    TEST_EQ(16, round_up_to_power_of_2(12));
    TEST_EQ(16, round_up_to_power_of_2(13));
    TEST_EQ(16, round_up_to_power_of_2(14));
    TEST_EQ(16, round_up_to_power_of_2(15));
    TEST_EQ(16, round_up_to_power_of_2(16));
    TEST_EQ(32, round_up_to_power_of_2(17));
    TEST_EQ(32, round_up_to_power_of_2(18));
    TEST_EQ(32, round_up_to_power_of_2(19));
    TEST_EQ(32, round_up_to_power_of_2(20));
    TEST_EQ(32, round_up_to_power_of_2(21));
    TEST_EQ(32, round_up_to_power_of_2(22));
    TEST_EQ(32, round_up_to_power_of_2(23));
    TEST_EQ(32, round_up_to_power_of_2(24));
    TEST_EQ(32, round_up_to_power_of_2(25));
    TEST_EQ(32, round_up_to_power_of_2(26));
    TEST_EQ(32, round_up_to_power_of_2(27));
    TEST_EQ(32, round_up_to_power_of_2(28));
    TEST_EQ(32, round_up_to_power_of_2(29));
    TEST_EQ(32, round_up_to_power_of_2(30));
    TEST_EQ(32, round_up_to_power_of_2(31));
    TEST_EQ(32, round_up_to_power_of_2(32));
}

DEFINE_TEST_SUITE(math) {
    TEST_ADD(test_round_up_to_power_of_2_powers());
    TEST_ADD(test_round_up_to_power_of_2_zero());
    TEST_ADD(test_round_up_to_power_of_2_all());
}
