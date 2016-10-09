#include "test_utils.h"
#include "../debug.h"
#include "../types.h"
#include "../list.h"
#include "../basic.h"


void test_list_1(void) {
    /* cons construction */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    TEST_EQ(1, CAR(a).val.integer_val);
    TEST_EQ(2, CAR(CDR(a)).val.integer_val);
    TEST_EQ(2, CAR(CDR(CDR(a))).val.integer_val);
}

void test_list_2(void) {
    /* length */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    Value length = list_length(a);
    TEST(length.type == INTEGER);
    TEST_EQ(length.val.integer_val, 3);
}


void test_list_3(void) {
    /* length of wrong*/
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS(VALUE_INTEGER(3),
                             VALUE_INTEGER(4))));
    Value length = list_length(a);
    TEST(length.type == ERROR);
}


void test_list_4(void) {
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

void test_list_5(void) {
    /* reverse */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(3))));
    Value b = CONS(VALUE_INTEGER(3),
                   CONS(VALUE_INTEGER(2),
                        CONS1(VALUE_INTEGER(1))));
    Value rev_a = list_reverse(a);
    Value rev_b = list_reverse(b);
    TEST(equal(rev_a, b));
    TEST(equal(rev_b, a));
}

void test_list_6(void) {
    /* reverse */
    Value a = CONS(VALUE_INTEGER(1),
                   CONS(VALUE_INTEGER(2),
                        CONS(VALUE_INTEGER(3),
                             VALUE_INTEGER(4))));
    Value rev = list_reverse(a);
    TEST(rev.type == ERROR);
    TEST(list_reverse(VALUE_INTEGER(5)).type == ERROR);
}
