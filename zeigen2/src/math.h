#ifndef Z_MATH_H
#define Z_MATH_H

#include "types.h"
#include <math.h>

#define RAD_TO_DEG(radians) ((radians) * 180.0 / M_PI)
#define DEG_TO_RAD(degree) ((degree) * M_PI / 180.0)
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

Unt round_up_to_power_of_2(Unt n);
#define MIN(x, y) (((x) <= (y)) ? (x) : (y))

Double random_int(Int lower, Int upper);
#endif
