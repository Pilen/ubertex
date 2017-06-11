#ifndef W_TEST_UTILS_H
#define W_TEST_UTILS_H

#include <stdio.h>

#include "../types.h"

Unt W_TEST_UTILS_count_total;
Unt W_TEST_UTILS_count_successful;
Bool W_TEST_UTILS_successful;

#define DEFINE_TEST_SUITE(name)                 \
    void W_TEST_UTILS_test_suite_##name(void)

#define TEST_ADD(test)                                                  \
    do {                                                                \
        W_TEST_UTILS_count_total++;                                     \
        test;                                                           \
        if (W_TEST_UTILS_successful) {                                  \
            W_TEST_UTILS_count_successful++;                            \
        }                                                               \
        W_TEST_UTILS_successful = true;                                 \
    } while (0)

#define DEFINE_TEST_PROGRAM                                \
    Unt W_TEST_UTILS_count_total = 0;                      \
    Unt W_TEST_UTILS_count_successful = 0;                 \
    Bool W_TEST_UTILS_successful = true;                   \
    int main(int argc __attribute__((unused)), char** argv __attribute__((unused))) { \
    printf("========\n");                                  \
    do {


#define TEST_USE_SUITE(name)                                        \
    Unt W_TEST_UTILS_test_suite_##name(void);                       \
    W_TEST_UTILS_test_suite_##name();                               \
    if (W_TEST_UTILS_count_successful < W_TEST_UTILS_count_total) { \
        break;                                                      \
    }                                                               \
    (void) 0;

#define END_TEST_PROGRAM                                                \
    } while (0);                                                        \
    if (W_TEST_UTILS_count_successful < W_TEST_UTILS_count_total) {     \
        printf("----\n");                                               \
        printf("TESTS FAILED!\n%d tests out of %d failed\n", W_TEST_UTILS_count_total - W_TEST_UTILS_count_successful, W_TEST_UTILS_count_total); \
        printf("========\n");                                           \
        return -1;                                                      \
    } else {                                                            \
        printf("%d/%d tests completed successfully.\n", W_TEST_UTILS_count_successful, W_TEST_UTILS_count_total); \
        printf("========\n");                                           \
        return 0;                                                       \
    }}


#define TEST(value)                                                     \
    do {                                                                \
        Int W_TEST_UTILS_test = (value);                                \
        if (!(W_TEST_UTILS_test)) {                                     \
            W_TEST_UTILS_successful = false;                            \
            printf("TEST-FAILED: %s:%d: %s: (%s)\t gave %d\n", __FILE__, __LINE__, __func__, #value, W_TEST_UTILS_test); \
            return;                                                     \
        }                                                               \
    } while (0);

#define TEST_M(value, ...)                                              \
    do {                                                                \
        Int W_TEST_UTILS_test = (value);                                \
        if (!(W_TEST_UTILS_test)) {                                     \
            W_TEST_UTILS_successful = false;                            \
            printf("TEST-FAILED: %s:%d: %s: (%s)\t gave %d\n\t", __FILE__, __LINE__, __func__, #value, W_TEST_UTILS_test); \
            printf(__VA_ARGS__);                                        \
            printf("\n");                                               \
            return;                                                     \
        }                                                               \
    } while (0);

#define TEST_EQ(a, b)                           \
    do {                                                                \
        if ((a) != (b)) {                                               \
            W_TEST_UTILS_successful = false;                            \
            printf("TEST-FAILED: %s:%d: %s: (%s == %s) gave (%i == %i)\n", __FILE__, __LINE__, __func__, #a, #b, (a), (b)); \
            return;                                                     \
        }                                                               \
    } while (0);
#endif
