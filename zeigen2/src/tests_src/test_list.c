
#include "test_utils.h"
#include "../debug.h"
#include "../types.h"
#include "../list.h"
#include "../basic.h"

void test_list_1(void) {
    List *list = list_create(1);
    TEST(list_length(list) == 0);
    list_push_back(list, VALUE_INTEGER(10));
    TEST(list_length(list) == 1);
    Value val = list_get(list, 0);
    TEST(val.val.integer_val == 10);
}

void test_list_2(void) {
    List *list = list_create(1);
    TEST(list_length(list) == 0);
    list_push_front(list, VALUE_INTEGER(10));
    TEST(list_length(list) == 1);
    Value val = list_get(list, 0);
    TEST(val.val.integer_val == 10);
}

void test_list_3(void) {
    List *list = list_create(4);
    TEST(list_length(list) == 0);
    list_push_back(list, VALUE_INTEGER(10));
    list_push_back(list, VALUE_INTEGER(20));
    list_push_back(list, VALUE_INTEGER(30));
    list_push_back(list, VALUE_INTEGER(40));
    TEST(list_length(list) == 4);
    TEST(list_get(list, 0).val.integer_val == 10);
    TEST(list_get(list, 1).val.integer_val == 20);
    TEST(list_get(list, 2).val.integer_val == 30);
    TEST(list_get(list, 3).val.integer_val == 40);
}

void test_list_4(void) {
    List *list = list_create(4);
    TEST(list_length(list) == 0);
    list_push_front(list, VALUE_INTEGER(10));
    list_push_front(list, VALUE_INTEGER(20));
    list_push_front(list, VALUE_INTEGER(30));
    list_push_front(list, VALUE_INTEGER(40));
    TEST(list_length(list) == 4);
    TEST(list_get(list, 0).val.integer_val == 40);
    TEST(list_get(list, 1).val.integer_val == 30);
    TEST(list_get(list, 3).val.integer_val == 10);
}

void test_list_5(void) {
    List *list = list_create_empty();
    TEST(list_length(list) == 0);
    list_push_back(list, VALUE_INTEGER(10));
    TEST(list_length(list) == 1);
    Value val = list_get(list, 0);
    TEST(val.val.integer_val == 10);
}

void test_list_6(void) {
    List *list = list_create_empty();
    TEST(list_length(list) == 0);
    TEST(list -> size == 0);
    for (Unt i = 0; i < 130; i++) {
        list_push_back(list, VALUE_INTEGER((i + 1) * 10));
        TEST_M(list_length(list) == i+1, "i = %d, length = %d", i, list_length(list));
    }
    for (Unt i = 0; i < 130; i++) {
        TEST_M(list_get(list, i).val.integer_val == (i + 1) * 10, "i = %d, length = %d", i, list_length(list));
    }
    TEST(list -> size == 256);
    for (Unt i = 0; i < 130; i++) {
        Value val = list_pop_back(list);
        TEST_M(list_length(list) == 130-i-1, "i = %d, length = %d", i, list_length(list));
        TEST(val.val.integer_val == (130 - i)*10);
    }
    TEST(list -> size == 1);

}

DEFINE_TEST_SUITE(list) {
    TEST_ADD(test_list_1());
    TEST_ADD(test_list_2());
    TEST_ADD(test_list_3());
    TEST_ADD(test_list_4());
    TEST_ADD(test_list_5());
    TEST_ADD(test_list_6());
}
