#include "test_utils.h"
#include "../debug.h"
#include "../types.h"
#include "../list.h"
#include "../basic.h"


void test_list_cons(void) {
    /* cons construction */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    TEST_EQ(1, CAR(a).val.integer_val);
    TEST_EQ(2, CAR(CDR(a)).val.integer_val);
    TEST_EQ(3, CAR(CDR(CDR(a))).val.integer_val);
}

void test_list_length(void) {
    /* length */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    Value length = list_length(a);
    TEST(length.type == INTEGER);
    TEST_EQ(length.val.integer_val, 3);
}


void test_list_length_wrong(void) {
    /* length of wrong*/
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS(VALUE_INTEGER(3),
                             VALUE_INTEGER(4))));
    Value length = list_length(a);
    TEST(length.type == ERROR);
}


void test_list_equal(void) {
    /* equal */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    Value b = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    Value c = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(4))));
    TEST(equal(a, a));
    TEST(equal(a, b));
    TEST(!equal(a, c));
}

void test_list_copy(void) {
    /* copy */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    Value b = list_copy(a);
    TEST(equal(a, b));
}

void test_list_copy_improper(void) {
    /* copy */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS(VALUE_INTEGER(3),
                             VALUE_INTEGER(4))));
    Value b = list_copy(a);
    TEST(b.type == ERROR);
}

void test_list_reverse1(void) {
    /* reverse */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    Value b = CONS(VALUE_INTEGER(3),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(1))));
    Value a_copy = list_copy(a);
    Value b_copy = list_copy(b);
    Value rev_a = list_reverse(a_copy);
    Value rev_b = list_reverse(b_copy);
    TEST(equal(rev_a, b));
    TEST(equal(rev_b, a));
}

void test_list_reverse2(void) {
    /* reverse */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS(VALUE_INTEGER(3),
                             VALUE_INTEGER(4))));
    LOG_DISABLE();
    Value rev = list_reverse(a);

    Value other = list_reverse(VALUE_INTEGER(5));
    LOG_REENABLE();
    TEST(rev.type == ERROR);
    TEST(other.type == ERROR);
}

DEFINE_TEST_SUITE(list) {
    TEST_ADD(test_list_cons());
    TEST_ADD(test_list_length());
    TEST_ADD(test_list_length_wrong());
    TEST_ADD(test_list_equal());
    TEST_ADD(test_list_copy());
    TEST_ADD(test_list_copy_improper());
    TEST_ADD(test_list_reverse1());
    TEST_ADD(test_list_reverse2());
}
