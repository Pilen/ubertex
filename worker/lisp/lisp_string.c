#include "../headers.h"

LISP_BUILTIN(format, "") {

    /* ENSURE_NOT_EMPTY(args); */
    /* Value format = NEXT(args); */
    /* ENSURE(format.type == String); */

    /* char *start = format.val.string_val -> text; */
    /* char *current = start; */

    /* Value result = CONS1(nil); */
    /* Value *end = &result; */
    /* while (true) { */
    /*     while (current != 0 && *current != '%') { */
    /*         current++; */
    /*     } */
    /*     if (current - 1> start) { */
    /*         String substring = string_create_from_substr(start, current - 1 - start); */
    /*         CDR(end) = CONS1(VALUE_STRING(substring)); */
    /*     } */
    /*     if (*current == 0) { */
    /*         break; */
    /*     } */
    /*     current++ */
    /*     if (*current == 0) { */
    /*         return VALUE_ERROR; */
    /*     } */
    /*     if (current == 's') { */
    /*         String converted = */
    /*     } */

    /* } */
    /* for (; *current != 0; text++) { */
    /*     for (t */
    /* } */
    return VALUE_ERROR;
}

LISP_BUILTIN(f, "") {
    ENSURE_NOT_EMPTY(args);
    Value format = NEXT(args);

    char *start = format.val.string_val -> text;
    char *run = start;
    char *current = start;

    Value list_beginning = CONS1(VALUE_NIL);
    Value *list = &list_beginning;

    while (true) {
        if (*current == STRING_END) {
            break;
        }
        if (*current == '{') {
            PUSH_END(list, VALUE_STRING(string_create_from_substr(run, current - run)));
            current++;
            run = current;
            while (true) {
                current++;
                if (*current == STRING_END) {
                    return VALUE_ERROR;
                }
                if (*current == '}') {
                    String *str = string_create_from_substr(run, current - run);
                    Value raw = read_from_str(str -> text);
                    Value evaled = eval(raw, environment);
                    ENSURE(evaled.type != ERROR);
                    Value string = VALUE_STRING(string_from_value(evaled, false));
                    PUSH_END(list, string);
                    run = current+1;
                    break;
                }
            }
        }
        current++;
    }
    PUSH_END(list, VALUE_STRING(string_create_from_substr(run, current - run)));

    String *result = string_flatten(CDR(list_beginning));
    return VALUE_STRING(result);
}


LISP_BUILTIN(split_lines, "") {
    ENSURE_NOT_EMPTY(args);
    Value string = NEXT(args);
    Value keep_ends_v = NEXT_DEFAULT(args, VALUE_NIL);
    ENSURE_EMPTY(args);

    Bool keep_ends = keep_ends_v.type != NIL;

    ENSURE(string.type == STRING);
    String *full = string.val.string_val;

    char *start = full -> text;
    char *current = start;
    Value result = CONS1(VALUE_NIL);
    Value *insert = &result;

    while (*current) {
        if (*current == '\n') {
            Unt bytes = current - start;
            if (keep_ends) {
                bytes++;
            }
            Value next = VALUE_STRING(string_create_from_substr(start, bytes));
            CAR(*insert) = next;
            CDR(*insert) = CONS1(VALUE_NIL);
            insert = &CDR(*insert);
            start = current+1;
        }
        current++;
    }
    Unt bytes = current - start;
    Value next = VALUE_STRING(string_create_from_substr(start, bytes));
    CAR(*insert) = next;
    return result;
}


LISP_BUILTIN(remove_chars, "") {
    ENSURE_NOT_EMPTY(args);
    Value text_v = NEXT(args);
    ENSURE_NOT_EMPTY(args);
    Value bad_v = NEXT(args);
    ENSURE_EMPTY(args);

    ENSURE(bad_v.type == STRING);
    ENSURE(text_v.type == STRING);

    char *bad = bad_v.val.string_val -> text;
    char *text = text_v.val.string_val -> text;
    char *buffer = NEW_BUFFER(char, text_v.val.string_val -> size);
    Unt insert = 0;
    for (Unt i = 0; text[i]; i++) {
        Bool is_bad = false;
        for (Unt j = 0; bad[j]; j++) {
            if (text[i] == bad[j]) {
                is_bad = true;
                break;
            }
        }

        if (!is_bad) {
            buffer[insert] = text[i];
            insert++;
        }
    }
    return VALUE_STRING(string_create_from_substr(buffer, insert));
}


LISP_BUILTIN(count_lines, "") {
    ENSURE_NOT_EMPTY(args);
    Value text_v = NEXT(args);
    ENSURE_EMPTY(args);
    ENSURE(text_v.type == STRING);

    Unt newlines = 1;
    char *text = text_v.val.string_val -> text;

    for (Unt i = 0; text[i]; i++) {
        if (text[i] == '\n') {
            newlines++;
        }
    }
    return VALUE_INTEGER(newlines);


}

LISP_BUILTIN(substring, "") {
    ENSURE_NOT_EMPTY(args);
    Value string_v = NEXT(args);
    Value from_v = NEXT_DEFAULT(args, VALUE_NIL);
    Value to_v = NEXT_DEFAULT(args, VALUE_NIL);
    ENSURE(string_v.type == STRING);
    ENSURE(from_v.type == INTEGER || from_v.type == NIL);
    ENSURE(to_v.type == INTEGER || to_v.type == NIL);
    String *string = string_v.val.string_val;
    Int length = string -> size - 1;
    Int from = from_v.type == INTEGER ? from = from_v.val.integer_val : 0;
    Int to = to_v.type == INTEGER ? to = to_v.val.integer_val : length;
    if (from < 0) {
        from = length + from;
    }
    if (to < 0) {
        to = length + to;
    }
    if (from > length) {
        from = length;
    }
    if (to > length) {
        to = length;
    }

    if (from < 0) {
        return VALUE_ERROR;
    }
    if (to < 0) {
        return VALUE_ERROR;
    }
    if (to < from) {
        return VALUE_ERROR;
    }

    Unt new_size = to - from + 1;
    debugi(from);
    debugi(to);
    debugi(length);
    debugi(new_size);
    String *new_string = memory_malloc(sizeof(String) + sizeof(char) * new_size);
    new_string -> refcount = 0;
    new_string -> size = new_size;
    Int i;
    Int j;
    for (i = from, j = 0; i < to; i++, j++) {
        char c = string -> text[i];
        debug("%i %c", i, c);
        if (!c) {debug("String ended prematurely");}
        new_string -> text[j] = c;
    }
    new_string -> text[j] = '\0';
    debug("%c", new_string -> text[0]);
    debug("%c", new_string -> text[1]);
    debug("%c", new_string -> text[2]);
    return VALUE_STRING(new_string);
}
