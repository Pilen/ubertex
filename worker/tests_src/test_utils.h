#ifndef Z_TEST_UTILS_H
#define Z_TEST_UTILS_H

#include <stdio.h>
#include "../types.h"

Unt Z_TEST_UTILS_count_total;
Unt Z_TEST_UTILS_count_successful;
Bool Z_TEST_UTILS_successful;

#define DEFINE_TEST_SUITE(name)                 \
    void Z_TEST_UTILS_test_suite_##name(void)

#define TEST_ADD(test)                                                  \
    do {                                                                \
        Z_TEST_UTILS_count_total++;                                     \
        test;                                                           \
        if (Z_TEST_UTILS_successful) {                                  \
            Z_TEST_UTILS_count_successful++;                            \
        }                                                               \
        Z_TEST_UTILS_successful = true;                                 \
    } while (0)

#define DEFINE_TEST_PROGRAM                                \
    Unt Z_TEST_UTILS_count_total = 0;                      \
    Unt Z_TEST_UTILS_count_successful = 0;                 \
    Bool Z_TEST_UTILS_successful = true;                   \
    int main(int argc, char** argv) {                      \
    printf("========\n");                                  \
    do {


#define TEST_USE_SUITE(name)                                        \
    Unt Z_TEST_UTILS_test_suite_##name(void);                       \
    Z_TEST_UTILS_test_suite_##name();                               \
    if (Z_TEST_UTILS_count_successful < Z_TEST_UTILS_count_total) { \
        break;                                                      \
    }                                                               \
    (void) 0;

#define END_TEST_PROGRAM                                                \
    } while (0);                                                        \
    if (Z_TEST_UTILS_count_successful < Z_TEST_UTILS_count_total) {     \
        printf("----\n");                                               \
        printf("TESTS FAILED!\n%d tests out of %d failed\n", Z_TEST_UTILS_count_total - Z_TEST_UTILS_count_successful, Z_TEST_UTILS_count_total); \
        printf("========\n");                                           \
        return -1;                                                      \
    } else {                                                            \
        printf("%d/%d tests completed successfully.\n", Z_TEST_UTILS_count_successful, Z_TEST_UTILS_count_total); \
        printf("========\n");                                           \
        return 0;                                                       \
    }}


#define TEST(value)                                                     \
    do {                                                                \
        Int Z_TEST_UTILS_test = (value);                                \
        if (!(Z_TEST_UTILS_test)) {                                     \
            Z_TEST_UTILS_successful = false;                            \
            printf("TEST-FAILED: %s:%d: %s: (%s)\t gave %d\n", __FILE__, __LINE__, __func__, #value, Z_TEST_UTILS_test); \
            return;                                                     \
        }                                                               \
    } while (0);

#define TEST_M(value, ...)                                              \
    do {                                                                \
        Int Z_TEST_UTILS_test = (value);                                \
        if (!(Z_TEST_UTILS_test)) {                                     \
            Z_TEST_UTILS_successful = false;                            \
            printf("TEST-FAILED: %s:%d: %s: (%s)\t gave %d\n\t", __FILE__, __LINE__, __func__, #value, Z_TEST_UTILS_test); \
            printf(__VA_ARGS__);                                        \
            printf("\n");                                               \
            return;                                                     \
        }                                                               \
    } while (0);

#endif
