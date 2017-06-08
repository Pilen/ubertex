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

#define TEST_RANDINT(var) {Int _test_randint_var = (var); if (0) {}
#define TEST_RANDINT_CASE(expected) else if ((_test_randint_var) == (expected)) {found_##expected = true;}
#define TEST_RANDINT_CASE_MINUS(expected) else if ((_test_randint_var) == -(expected)) {found_m##expected = true;}
#define TEST_RANDINT_END else {debugi(_test_randint_var); TEST(0);}}

void test_randint_0_3(void) {
    Bool found_0 = false;
    Bool found_1 = false;
    Bool found_2 = false;
    for (Unt i = 0; i < 1000000000; i++) {
        Int r = random_int(0, 3);
        TEST_RANDINT(r)
            TEST_RANDINT_CASE(0)
            TEST_RANDINT_CASE(1)
            TEST_RANDINT_CASE(2)
            TEST_RANDINT_END;

        if (i >= 10 && found_0 && found_1 && found_2){
            return;
        }
    }
    TEST(0);
}

void test_randint_m1_3(void) {
    Bool found_m1 = false;
    Bool found_0 = false;
    Bool found_1 = false;
    Bool found_2 = false;
    for (Unt i = 0; i < 1000000000; i++) {
        Int r = random_int(-1, 3);
        TEST_RANDINT(r)
            TEST_RANDINT_CASE_MINUS(1)
            TEST_RANDINT_CASE(0)
            TEST_RANDINT_CASE(1)
            TEST_RANDINT_CASE(2)
            TEST_RANDINT_END;

        if (i >= 10 && found_m1 && found_0 && found_1 && found_2){
            return;
        }
    }
    TEST(0);
}

void test_randint_m3_1(void) {
    Bool found_m3 = false;
    Bool found_m2 = false;
    Bool found_m1 = false;
    Bool found_0 = false;
    for (Unt i = 0; i < 1000000000; i++) {
        Int r = random_int(-3, 1);
        TEST_RANDINT(r)
            TEST_RANDINT_CASE_MINUS(3)
            TEST_RANDINT_CASE_MINUS(2)
            TEST_RANDINT_CASE_MINUS(1)
            TEST_RANDINT_CASE(0)
            TEST_RANDINT_END;

        if (i >= 10 && found_m3 && found_m2 && found_m1 && found_0){
            return;
        }
    }
    TEST(0);
}

void test_randint_m3_0(void) {
    Bool found_m3 = false;
    Bool found_m2 = false;
    Bool found_m1 = false;
    for (Unt i = 0; i < 1000000000; i++) {
        Int r = random_int(-3, 0);
        TEST_RANDINT(r)
            TEST_RANDINT_CASE_MINUS(3)
            TEST_RANDINT_CASE_MINUS(2)
            TEST_RANDINT_CASE_MINUS(1)
            TEST_RANDINT_END;

        if (i >= 10 && found_m3 && found_m2 && found_m1){
            return;
        }
    }
    TEST(0);
}

void test_randint_m3_m1(void) {
    Bool found_m3 = false;
    Bool found_m2 = false;
    for (Unt i = 0; i < 1000000000; i++) {
        Int r = random_int(-3, -1);
        TEST_RANDINT(r)
            TEST_RANDINT_CASE_MINUS(3)
            TEST_RANDINT_CASE_MINUS(2)
            TEST_RANDINT_END;

        if (i >= 10 && found_m3 && found_m2){
            return;
        }
    }
    TEST(0);
}

void test_randint_1_1(void) {
    Bool found_1 = false;
    for (Unt i = 0; i < 1000000000; i++) {
        Int r = random_int(1, 2);
        TEST_RANDINT(r)
            TEST_RANDINT_CASE(1)
            TEST_RANDINT_END;

        if (i >= 10 && found_1) {
            return;
        }
    }
    TEST(0);
}

void test_randint_0_0(void) {
    Bool found_0 = false;
    for (Unt i = 0; i < 1000000000; i++) {
        Int r = random_int(0, 1);
        TEST_RANDINT(r)
            TEST_RANDINT_CASE(0)
            TEST_RANDINT_END;

        if (i >= 10 && found_0) {
            return;
        }
    }
    TEST(0);
}

DEFINE_TEST_SUITE(math) {
    TEST_ADD(test_round_up_to_power_of_2_powers());
    TEST_ADD(test_round_up_to_power_of_2_zero());
    TEST_ADD(test_round_up_to_power_of_2_all());
    TEST_ADD(test_randint_0_3());
    TEST_ADD(test_randint_m1_3());
    TEST_ADD(test_randint_m3_1());
    TEST_ADD(test_randint_m3_0());
    TEST_ADD(test_randint_m3_m1());
    TEST_ADD(test_randint_1_1());
}
