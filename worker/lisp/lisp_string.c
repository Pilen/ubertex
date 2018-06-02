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
