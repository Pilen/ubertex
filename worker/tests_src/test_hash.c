#include "test_utils.h"
#include "../debug.h"
#include "../types.h"
#include "../hash.h"
#include "../basic.h"
#include "../math.h"

void test_hash_1(void) {
    Hash *hash = hash_create();
    TEST(hash_length(hash) == 0);
    Value key = VALUE_INTEGER(10);
    Value data = VALUE_INTEGER(1000);
    hash_set(hash, key, data);

    Value result;
    Bool found = hash_get(hash, data, &result);
    TEST(!found);
    found = hash_get(hash, key, &result);
    TEST(found);
    TEST(result.val.integer_val == data.val.integer_val);
}

void test_hash_2(void) {
    Hash *hash = hash_create();
    Int length = HASH_DEFAULT_SIZE - 1;
    for (Int i = 0; i < length; i++) {
        Value key = VALUE_FLOAT(10.0 * i);
        Value data = VALUE_INTEGER(1000 * i);
        hash_set(hash, key, data);
    }
    TEST(hash_length(hash) == length);
    for (Int i = 0; i < length; i++) {
        Value key = VALUE_FLOAT(10.0 * i);
        Value data;
        Bool found = hash_get(hash, key, &data);
        TEST(found);
        TEST(data.val.integer_val == 1000 * i);
    }
}

void test_hash_3(void) {
    Hash *hash = hash_create();
    Value key = VALUE_INTEGER(100);
    for (Unt i = 0; i < 200; i++) {
        Value data = VALUE_INTEGER(i + 900);
        hash_set(hash, key, data);
        TEST(hash -> length == 1);
    }
}

void test_hash_4(void) {
    Hash *hash = hash_create();
    Unt length = 200;
    for (Unt i = 0; i < length; i++) {
        Value key = VALUE_INTEGER(i);
        Value data = VALUE_INTEGER(i+1000);
        hash_set(hash, key, data);
        TEST(hash -> length == i + 1);
    }
    TEST(hash -> length == length);
    TEST(hash -> size == round_up_to_power_of_2(length));
}

DEFINE_TEST_SUITE(hash) {
    TEST_ADD(test_hash_1());
    TEST_ADD(test_hash_2());
    TEST_ADD(test_hash_3());
    TEST_ADD(test_hash_4());
}
