#ifndef W_COLOR_H
#define W_COLOR_H

struct Color_s {
    Double r;
    Double g;
    Double b;
    Double a;
};

Color *color_create_rgb(Value red, Value green, Value blue);
Color *color_create_rgba(Value red, Value green, Value blue, Value alpha);

#endif
