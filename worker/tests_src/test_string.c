#include "test_utils.h"
#include "../headers.h"

void test_string_value_to_string1(void) {
    Value lisp = VALUE_NIL;
    Value expected = VALUE_STRING(string_create_from_str("nil"));
    Value string = VALUE_STRING(string_from_value(lisp));
    TEST(equal(string, expected));
}

void test_string_value_to_string2(void) {
    Value lisp = CONS(VALUE_NIL, VALUE_NIL);
    Value expected = VALUE_STRING(string_create_from_str("(nil)"));
    Value string = VALUE_STRING(string_from_value(lisp));
    TEST(equal(string, expected));
}

void test_string_value_to_string3(void) {
    Value lisp = CONS(VALUE_INTEGER(1), VALUE_INTEGER(2));
    Value expected = VALUE_STRING(string_create_from_str("(1 . 2)"));
    Value string = VALUE_STRING(string_from_value(lisp));
    TEST(equal(string, expected));
}

void test_string_value_to_string4(void) {
    Value lisp = CONS(VALUE_INTEGER(1), CONS(VALUE_INTEGER(2), VALUE_INTEGER(3)));
    Value expected = VALUE_STRING(string_create_from_str("(1 2 . 3)"));
    Value string = VALUE_STRING(string_from_value(lisp));
    TEST(equal(string, expected));
}

void test_string_value_to_string5(void) {
    Value lisp =
        CONS(VALUE_ERROR,
             CONS(VALUE_NIL,
                  CONS(symbols_progn,
                       CONS(VALUE_INTEGER(10),
                            CONS(VALUE_FLOAT(3.14),
                                 CONS(VALUE_STRING(string_create_from_str("abc")),
                                      CONS(CONS(VALUE_INTEGER(1), VALUE_INTEGER(2)),
                                           CONS1(VALUE_INTEGER(0)))))))));
    Value expected = VALUE_STRING(string_create_from_str("(error nil progn 10 3.14 \"abc\" (1 . 2) 0)"));
    Value string = VALUE_STRING(string_from_value(lisp));
    TEST(equal(string, expected));
}

DEFINE_TEST_SUITE(string) {
    TEST_ADD(test_string_value_to_string1());
    TEST_ADD(test_string_value_to_string2());
    TEST_ADD(test_string_value_to_string3());
    TEST_ADD(test_string_value_to_string4());
    TEST_ADD(test_string_value_to_string5());
}
