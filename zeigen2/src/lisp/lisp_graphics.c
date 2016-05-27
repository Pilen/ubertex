
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
    Int *colors = memory_malloc(sizeof(Int) * 4);
    if (args -> length == 2) {
        Value color = LIST_GET_UNSAFE(args, 1);
        if (color.type == INTEGER) {
            colors[0] = color.val.integer_val;
            colors[1] = color.val.integer_val;
            colors[2] = color.val.integer_val;
            colors[3] = 255;
        } else if (color.type == STRING) {
            /* TODO: parse color string "#aabbcc[dd]" */
            return VALUE_ERROR;
        } else {
            return VALUE_ERROR;
        }
    } else if (args -> length == 4) {
        Value red = LIST_GET_UNSAFE(args, 1);
        Value green = LIST_GET_UNSAFE(args, 2);
        Value blue = LIST_GET_UNSAFE(args, 3);
        if (red.type == INTEGER && green.type == INTEGER &&
            blue.type == INTEGER) {
            colors[0] = red.val.integer_val;
            colors[1] = green.val.integer_val;
            colors[2] = blue.val.integer_val;
            colors[3] = 255;
        } else {
            return VALUE_ERROR;
        }
    } else if (args -> length == 5) {
        Value red = LIST_GET_UNSAFE(args, 1);
        Value green = LIST_GET_UNSAFE(args, 2);
        Value blue = LIST_GET_UNSAFE(args, 3);
        Value alpha = LIST_GET_UNSAFE(args, 4);
        if (red.type == INTEGER && green.type == INTEGER &&
            blue.type ==  INTEGER && alpha.type == INTEGER) {
            colors[0] = red.val.integer_val;
            colors[1] = green.val.integer_val;
            colors[2] = blue.val.integer_val;
            colors[3] = alpha.val.integer_val;
        } else {
            return VALUE_ERROR;
        }
    } else {
        return VALUE_ERROR;
    }

    return VALUE_VECTOR4I(colors);
}

LISP_BUILTIN(clear_color, "") {
    if (args -> length == 1) {
        Int *colors = memory_malloc(sizeof(Int) * 4);
        colors[0] = environment -> clear_red;
        colors[1] = environment -> clear_green;
        colors[2] = environment -> clear_blue;
        colors[3] = environment -> clear_alpha;
        return VALUE_VECTOR4I(colors);
    } else if (args -> length == 2) {
        Value vector = LIST_GET_UNSAFE(args, 1);
        if (vector.type != VECTOR4I) {
            return VALUE_ERROR;
        }
        Int *colors = vector.val.vector4i_val;
        environment -> clear_red = colors[0];
        environment -> clear_green = colors[1];
        environment -> clear_blue = colors[2];
        environment -> clear_alpha = colors[3];

        return vector;
    }
    return VALUE_ERROR;
}

LISP_BUILTIN(clear, "") {
    /* TODO: if called with a color, clear with that color without setting normal clear color */
    graphics_clear(environment);
    return VALUE_NIL;
}

LISP_BUILTIN(fill, "") {
    if (args -> length != 2) {
        return VALUE_ERROR;
    }
    Value color = LIST_GET_UNSAFE(args, 1);
    if (color.type != VECTOR4I) {
        return VALUE_ERROR;
    }
    Double *vector = color.val.vector4f_val;
    graphics_fill(environment, vector[0], vector[1], vector[2], vector[3]);
    return VALUE_NIL;
}

LISP_BUILTIN(image, "") {
    if (args -> length != 3) {
        return VALUE_ERROR;
    }

    Value file = LIST_GET_UNSAFE(args, 1);
    Value position = LIST_GET_UNSAFE(args, 2);

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
    if (args -> length != 3 && args -> length != 4) {
        return VALUE_ERROR;
    }

    Value file = LIST_GET_UNSAFE(args, 1);
    if (file.type != STRING) {
        return VALUE_ERROR;
    }
    Value slide = LIST_GET_UNSAFE(args, 2);
    if (slide.type != INTEGER) {
        return VALUE_ERROR;
    }

    Value position = VALUE_NIL;
    if (args -> length == 4) {
        position = LIST_GET_UNSAFE(args, 3);
    }

    Renderable renderable;
    if (!pdf_get_slide(environment, file, slide.val.integer_val, &renderable)) {
        return VALUE_ERROR;
    }
    graphics_render_at_position(environment, &renderable, position);
    return symbols_t;
}

LISP_BUILTIN(text, "") {
    return VALUE_ERROR;
    /* /\* (text string &optional fontsize centered position) *\/ */

    /* /\* TODO: The arguments should be interpreted better *\/ */
    /* Int fontsize = 34; */
    /* Value position = VALUE_NIL; */
    /* Bool center = true; */

    /* if (args -> length >= 3) { */
    /*     Value fontsize_v = LIST_GET_UNSAFE(args, 2); */
    /*     if (fontsize_v.type == INTEGER) { */
    /*         fontsize = fontsize_v.val.integer_val; */
    /*     } else { */
    /*         return VALUE_ERROR; */
    /*     } */
    /* } */
    /* if (args -> length >= 4) { */
    /*     Value center_v = LIST_GET_UNSAFE(args, 3); */
    /*     center = center_v.type != NIL; */
    /* } */
    /* if (args -> length == 5) { */
    /*     position = LIST_GET_UNSAFE(args, 4); */
    /* } */
    /* if (args -> length < 2 || args -> length > 5) { */
    /*     return VALUE_ERROR; */
    /* } */

    /* Value string = LIST_GET_UNSAFE(args, 1); */
    /* if (string.type != STRING) { */
    /*     return VALUE_ERROR; */
    /* } */

    /* SDL_SetRenderDrawColor(environment -> renderer, 255, 125, 255, 255); */
    /* SDL_Texture *texture = text(environment, string.val.string_val, fontsize, center); */
    /* if (!texture) { */
    /*     return VALUE_ERROR; */
    /* } */

    /* if (position.type == NIL) { */
    /*     Int width; */
    /*     Int height; */
    /*     SDL_GetWindowSize(environment -> window, &width, &height); */

    /*     graphics_render_centered_at(environment, texture, width/2, height/2); */
    /* } else { */
    /*     graphics_render_at_position(environment, texture, position); */
    /* } */
    /* return VALUE_NIL; */
}

LISP_BUILTIN(calibrate, "") {
    graphics_calibrate(environment);
    return VALUE_NIL;
}
