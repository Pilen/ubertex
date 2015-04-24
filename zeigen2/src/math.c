
#include "math.h"

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
