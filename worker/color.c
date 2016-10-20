
#include "color.h"
#include "memory.h"

Color *color_create_rgb(Value red, Value green, Value blue) {
    return color_create_rgba(red, green, blue, VALUE_FLOAT(1.0));
}

Color *color_create_rgba(Value red, Value green, Value blue, Value alpha) {
    Double r;
    Double g;
    Double b;
    Double a;
    if (red.type == INTEGER) {
        r = (red.val.integer_val * 1.0) / 255.0;
    } else if (red.type == FLOAT) {
        r = red.val.float_val;
    } else {
        return NULL;
    }
    if (green.type == INTEGER) {
        g = (green.val.integer_val * 1.0) / 255.0;
    } else if (green.type == FLOAT) {
        g = green.val.float_val;
    } else {
        return NULL;
    }
    if (blue.type == INTEGER) {
        b = (blue.val.integer_val * 1.0) / 255.0;
    } else if (blue.type == FLOAT) {
        b = blue.val.float_val;
    } else {
        return NULL;
    }
    if (alpha.type == INTEGER) {
        a = (alpha.val.integer_val * 1.0) / 255.0;
    } else if (alpha.type == FLOAT) {
        a = alpha.val.float_val;
    } else {
        return NULL;
    }
    Color *color = memory_malloc(sizeof(Color));
    color -> r = r;
    color -> g = g;
    color -> b = b;
    color -> a = a;
    return color;
}
