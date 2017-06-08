
#include <stdlib.h>

#include "libs/pcg-c-basic-0.9/pcg_basic.h"

#include "headers.h"

Unt round_up_to_power_of_2(Unt n) {
    /* If n is a power of two, return it */
    if ((n & (n - 1)) == 0) {
        return n;
    } else {
        Unt power = 1;
        while (power < n) {
            power *= 2;
        }
        return power;
    }
}

Int random_int(Int lower, Int upper) {
    /* lower inclusive, upper exclusive */
    /* upper--; */
    w_assert(lower <= upper);
    Int limit = upper - lower;
    return pcg32_boundedrand(limit) + lower;
}
