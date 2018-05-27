#ifndef W_MATH_H
#define W_MATH_H

#include <math.h>

#define RAD_TO_DEG(radians) ((radians) * 180.0 / M_PI)
#define DEG_TO_RAD(degree) ((degree) * M_PI / 180.0)
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_E
#define M_E 2.7182818284590452354
#endif

Unt round_up_to_power_of_2(Unt n);
#ifndef MIN
#define MIN(x, y) (((x) <= (y)) ? (x) : (y))
#endif

Int random_int(Int lower, Int upper);
void random_seed(Unt seed);

#endif
