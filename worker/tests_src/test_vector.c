
#include "test_utils.h"
#include "../headers.h"

void test_vector_1(void) {
    Vector *vector = vector_create(1);
    TEST(vector_length(vector) == 0);
    vector_push_back(vector, VALUE_INTEGER(10));
    TEST(vector_length(vector) == 1);
    Value val = vector_get(vector, 0);
    TEST(val.val.integer_val == 10);
}

void test_vector_2(void) {
    Vector *vector = vector_create(1);
    TEST(vector_length(vector) == 0);
    vector_push_front(vector, VALUE_INTEGER(10));
    TEST(vector_length(vector) == 1);
    Value val = vector_get(vector, 0);
    TEST(val.val.integer_val == 10);
}

void test_vector_3(void) {
    Vector *vector = vector_create(4);
    TEST(vector_length(vector) == 0);
    vector_push_back(vector, VALUE_INTEGER(10));
    vector_push_back(vector, VALUE_INTEGER(20));
    vector_push_back(vector, VALUE_INTEGER(30));
    vector_push_back(vector, VALUE_INTEGER(40));
    TEST(vector_length(vector) == 4);
    TEST(vector_get(vector, 0).val.integer_val == 10);
    TEST(vector_get(vector, 1).val.integer_val == 20);
    TEST(vector_get(vector, 2).val.integer_val == 30);
    TEST(vector_get(vector, 3).val.integer_val == 40);
}

void test_vector_4(void) {
    Vector *vector = vector_create(4);
    TEST(vector_length(vector) == 0);
    vector_push_front(vector, VALUE_INTEGER(10));
    vector_push_front(vector, VALUE_INTEGER(20));
    vector_push_front(vector, VALUE_INTEGER(30));
    vector_push_front(vector, VALUE_INTEGER(40));
    TEST(vector_length(vector) == 4);
    TEST(vector_get(vector, 0).val.integer_val == 40);
    TEST(vector_get(vector, 1).val.integer_val == 30);
    TEST(vector_get(vector, 3).val.integer_val == 10);
}

void test_vector_5(void) {
    Vector *vector = vector_create_empty();
    TEST(vector_length(vector) == 0);
    vector_push_back(vector, VALUE_INTEGER(10));
    TEST(vector_length(vector) == 1);
    Value val = vector_get(vector, 0);
    TEST(val.val.integer_val == 10);
}

void test_vector_6(void) {
    Vector *vector = vector_create_empty();
    TEST(vector_length(vector) == 0);
    TEST(vector -> size == 0);
    for (Unt i = 0; i < 130; i++) {
        vector_push_back(vector, VALUE_INTEGER((i + 1) * 10));
        TEST_M(vector_length(vector) == i+1, "i = %d, length = %d", i, vector_length(vector));
    }
    for (Unt i = 0; i < 130; i++) {
        TEST_M(vector_get(vector, i).val.integer_val == (i + 1) * 10, "i = %d, length = %d", i, vector_length(vector));
    }
    TEST(vector -> size == 256);
    for (Unt i = 0; i < 130; i++) {
        Value val = vector_pop_back(vector);
        TEST_M(vector_length(vector) == 130-i-1, "i = %d, length = %d", i, vector_length(vector));
        TEST(val.val.integer_val == (130 - i)*10);
    }
    TEST(vector -> size == 1);
}

void test_vector_7(void) {
    Vector *vector = vector_create(6);
    for (Unt i = 0; i < 6; i++) {
        vector_push_back(vector, VALUE_INTEGER(i));
    }

    TEST_EQ(vector_length(vector), 6);
    TEST_EQ(vector -> size, 6);
    vector_pop_front(vector);
    vector_pop_front(vector);
    TEST_EQ(vector_length(vector), 4);
    TEST_EQ(vector -> size, 6);
}

void test_vector_8(void) {
    Vector *vector = vector_create(6);
    for (Unt i = 0; i < 6; i++) {
        vector_push_back(vector, VALUE_INTEGER(i));
    }
    vector_pop_front(vector);
    vector_pop_back(vector);
    vector_pop_back(vector);
    TEST_EQ(vector_length(vector), 3);
    TEST(vector_get(vector, 0).val.integer_val == 1);
    TEST(vector_get(vector, 1).val.integer_val == 2);
    TEST(vector_get(vector, 2).val.integer_val == 3);
    TEST_EQ(vector -> start, 1); // Whitebox
}

void test_vector_9(void) {
    Vector *vector = vector_create(6);
    for (Unt i = 0; i < 3; i++) {
        vector_push_back(vector, VALUE_INTEGER(i*10));
        vector_push_front(vector, VALUE_INTEGER(i));
    }
    vector_normalize(vector);
    TEST_EQ(vector_length(vector), 6);
    TEST_EQ(vector -> size, 6);
    TEST_EQ(vector -> start, 0);
}

DEFINE_TEST_SUITE(vector) {
    TEST_ADD(test_vector_1());
    TEST_ADD(test_vector_2());
    TEST_ADD(test_vector_3());
    TEST_ADD(test_vector_4());
    TEST_ADD(test_vector_5());
    TEST_ADD(test_vector_6());
    TEST_ADD(test_vector_7());
    TEST_ADD(test_vector_8());
    TEST_ADD(test_vector_9());
}
