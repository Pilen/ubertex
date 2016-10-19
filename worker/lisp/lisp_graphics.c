
#include "../types.h"
#include "../eval.h"
#include "../list.h"
#include "../basic.h"
#include "../symbol.h"
#include "../math.h"
#include "../eval.h"
#include "../debug.h"
#include "../memory.h"
#include "../image.h"
#include "../graphics.h"
#include "../pdf.h"
#include "../text.h"

LISP_BUILTIN(color, "") {
    return VALUE_ERROR;
    /* Int *colors = memory_malloc(sizeof(Int) * 4); */
    /* if (args -> length == 2) { */
    /*     Value color = LIST_GET_UNSAFE(args, 1); */
    /*     if (color.type == INTEGER) { */
    /*         colors[0] = color.val.integer_val; */
    /*         colors[1] = color.val.integer_val; */
    /*         colors[2] = color.val.integer_val; */
    /*         colors[3] = 255; */
    /*     } else if (color.type == STRING) { */
    /*         /\* TODO: parse color string "#aabbcc[dd]" *\/ */
    /*         return VALUE_ERROR; */
    /*     } else { */
    /*         return VALUE_ERROR; */
    /*     } */
    /* } else if (args -> length == 4) { */
    /*     Value red = LIST_GET_UNSAFE(args, 1); */
    /*     Value green = LIST_GET_UNSAFE(args, 2); */
    /*     Value blue = LIST_GET_UNSAFE(args, 3); */
    /*     if (red.type == INTEGER && green.type == INTEGER && */
    /*         blue.type == INTEGER) { */
    /*         colors[0] = red.val.integer_val; */
    /*         colors[1] = green.val.integer_val; */
    /*         colors[2] = blue.val.integer_val; */
    /*         colors[3] = 255; */
    /*     } else { */
    /*         return VALUE_ERROR; */
    /*     } */
    /* } else if (args -> length == 5) { */
    /*     Value red = LIST_GET_UNSAFE(args, 1); */
    /*     Value green = LIST_GET_UNSAFE(args, 2); */
    /*     Value blue = LIST_GET_UNSAFE(args, 3); */
    /*     Value alpha = LIST_GET_UNSAFE(args, 4); */
    /*     if (red.type == INTEGER && green.type == INTEGER && */
    /*         blue.type ==  INTEGER && alpha.type == INTEGER) { */
    /*         colors[0] = red.val.integer_val; */
    /*         colors[1] = green.val.integer_val; */
    /*         colors[2] = blue.val.integer_val; */
    /*         colors[3] = alpha.val.integer_val; */
    /*     } else { */
    /*         return VALUE_ERROR; */
    /*     } */
    /* } else { */
    /*     return VALUE_ERROR; */
    /* } */

    /* return VALUE_VECTOR4I(colors); */
}

LISP_BUILTIN(clear_color, "") {
    return VALUE_ERROR;
    /* if (args -> length == 1) { */
    /*     Int *colors = memory_malloc(sizeof(Int) * 4); */
    /*     colors[0] = environment -> clear_red; */
    /*     colors[1] = environment -> clear_green; */
    /*     colors[2] = environment -> clear_blue; */
    /*     colors[3] = environment -> clear_alpha; */
    /*     return VALUE_VECTOR4I(colors); */
    /* } else if (args -> length == 2) { */
    /*     Value vector = LIST_GET_UNSAFE(args, 1); */
    /*     if (vector.type != VECTOR4I) { */
    /*         return VALUE_ERROR; */
    /*     } */
    /*     Int *colors = vector.val.vector4i_val; */
    /*     environment -> clear_red = colors[0]; */
    /*     environment -> clear_green = colors[1]; */
    /*     environment -> clear_blue = colors[2]; */
    /*     environment -> clear_alpha = colors[3]; */

    /*     return vector; */
    /* } */
    /* return VALUE_ERROR; */
}

LISP_BUILTIN(clear, "") {
    /* TODO: if called with a color, clear with that color without setting normal clear color */
    graphics_clear(environment);
    return VALUE_NIL;
}

LISP_BUILTIN(fill, "") {
    ENSURE_NOT_EMPTY(args);
    Value r = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value g = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value b = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value a = NEXT(args);
    if (!IS_NUMERIC(r) ||
        !IS_NUMERIC(g) ||
        !IS_NUMERIC(b) ||
        !IS_NUMERIC(a)) {
        return VALUE_ERROR;
    }
    graphics_fill(environment, NUM_VAL(r), NUM_VAL(g), NUM_VAL(b), NUM_VAL(a));
    return VALUE_NIL;

    /* if (args -> length != 2) { */
    /*     return VALUE_ERROR; */
    /* } */
    /* Value color = LIST_GET_UNSAFE(args, 1); */
    /* if (color.type != VECTOR4I) { */
    /*     return VALUE_ERROR; */
    /* } */
    /* Double *vector = color.val.vector4f_val; */
    /* graphics_fill(environment, vector[0], vector[1], vector[2], vector[3]); */
    /* return VALUE_NIL; */
}

LISP_BUILTIN(image, "") {
    ENSURE_NOT_EMPTY(args);

    Value file = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value position = NEXT(args);

    Renderable renderable;
    if (!image_get_renderable_from_file(environment, file, &renderable)) {
        return VALUE_ERROR;
    }
    if (graphics_render_at_position(environment, &renderable, position)) {
        return symbols_t;
    } else {
        return VALUE_ERROR;
    }
}

LISP_BUILTIN(pdf, "") {
    ENSURE_NOT_EMPTY(args);
    Value file = NEXT(args);
    if (file.type != STRING) {
        return VALUE_ERROR;
    }
    ENSURE_NOT_EMPTY(args);
    Value slide = NEXT(args);
    if (slide.type != INTEGER) {
        return VALUE_ERROR;
    }

    Value position = VALUE_NIL;
    if (args.type == CONS) {
        position = NEXT(args);
    }

    Renderable renderable;
    if (!pdf_get_slide(environment, file, slide.val.integer_val, &renderable)) {
        return VALUE_ERROR;
    }
    graphics_render_at_position(environment, &renderable, position);
    return symbols_t;
}

LISP_BUILTIN(text, "") {
    /* (text string &optional fontsize centered position) */

    /* TODO: The arguments should be interpreted better */
    Int fontsize = -1;
    Bool align_center = true;
    Value position = VALUE_NIL;

    ENSURE_NOT_EMPTY(args);
    Value string = NEXT(args);
    if (string.type != STRING) {
        return VALUE_ERROR;
    }

    if (args.type == CONS) {
        Value fontsize_v = NEXT(args);
        if (fontsize_v.type == INTEGER) {
            fontsize = fontsize_v.val.integer_val;
        } else {
            return VALUE_ERROR;
        }
    }
    if (args.type == CONS) {
        Value center_v = NEXT(args);
        align_center = center_v.type != NIL;
    }
    if (args.type == CONS) {
        position = NEXT(args);
    }
    ENSURE_EMPTY(args);

    cairo_set_source_rgb(environment -> cairo, 1.0, 1.0, 1.0);
    Renderable renderable;
    if (!text(environment, string.val.string_val, fontsize, align_center, &renderable)) {
        return VALUE_ERROR;
    }

    if (position.type == NIL) {
        graphics_render_centered_at(environment, &renderable, environment -> width/2, environment -> height/2);
    } else {
        graphics_render_at_position(environment, &renderable, position);
    }
    return VALUE_NIL;
}

LISP_BUILTIN(calibrate, "") {
    graphics_calibrate(environment);
    return VALUE_NIL;
}
