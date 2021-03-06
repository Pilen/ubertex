#include "debug.h"

#include <stdio.h>
#include <string.h>

#include "headers.h"

/* TODO: this file does not work with unicode yet!!! */

Bool read_script(char **code, char *end, Unt *linenumber, Value *result);
Bool read_expression(char **code, char *end, Unt *linenumber, Value *result);
Bool read_munch_whitespace(char **code, char *end, Unt *linenumber);
Bool read_munch_comment(char **code, char *end, Unt *linenumber);
Bool read_integer(char **code, char *end, Unt *linenumber, Value *result);
Bool read_float(char **code, char *end, Unt *linenumber, Value *result);
Bool read_string(char **code, char *end, Unt *linenumber, Value *result);
Bool read_symbol(char **code, char *end, Unt *linenumber, Value *result);
Bool read_list(char **code, char *end, Unt *linenumber, Value *result);
Bool read_quote(char **code, char *end, Unt *linenumber, Value *result);
Bool read_at_delimiter(char *character, char *end);
Bool read_char_exists_in(char character, char* text);

Value read_value(Value value) {
    if (value.type != STRING) {
        return VALUE_ERROR;
    }
    return read_from_str(value.val.string_val -> text);
}

Value read_from_str(char *str) {
    log_section("====READ====");
    char *code = str;
    char *end = code + strlen(code);
    /* debugv("code = %p, end = %p, diff=%td", code, end, end-code); */
    Unt linenumber = 0;
    Value result;
    Bool found = read_script(&code, end, &linenumber, &result);
    log_section("====READ-END====");
    if (found && code == end) {
        return result;
    }
    if (found) {
        log_error("Read something, but not all");
    }
    /* TODO: log error */
    return VALUE_ERROR;
}


Bool read_script(char **code, char *end, Unt *linenumber, Value *result) {
    char *p = *code;
    Value script = CONS1(symbols_progn);
    Value last = script;
    Unt count = 0;

    read_munch_whitespace(&p, end, linenumber);
    while (p < end) {
        Value value;
        if (!read_expression(&p, end, linenumber, &value)) {
            log_error("could not read expression: %s", p);
            break;
        }
        read_munch_whitespace(&p, end, linenumber);
        count++;
        CDR(last) = CONS1(value);
        last = CDR(last);
    }
    if (p != end) {
        /* list_destroy(script); */
        return false;
    }
    if (count == 1) {
        script = CAR(CDR(script));
    }
    *result = script;
    *code = p;
    return true;
}

/* End point to char just beyond the last printable, aka nul */
Bool read_expression(char **code, char *end, Unt *linenumber, Value *result) {
    /* Skip spaces */
    /* read_munch_whitespace(code, end, linenumber); */
    if (read_float(code, end, linenumber, result)) {
        return true;
    }
    if (read_integer(code, end, linenumber, result)) {
        return true;
    }
    if (read_symbol(code, end, linenumber, result)) {
        return true;
    }
    if (read_string(code, end, linenumber, result)) {
        return true;
    }
    if (read_list(code, end, linenumber, result)) {
        return true;
    }
    if (read_quote(code, end, linenumber, result)) {
        return true;
    }

    return false;
    /* return */
    /*     read_float(code, end, linenumber, result)   || */
    /*     read_integer(code, end, linenumber, result) || */
    /*     read_string(code, end, linenumber, result)  || */
    /*     read_symbol(code, end, linenumber, result)  || */
    /*     read_list(code, end, linenumber, result); */
}

Bool read_munch_whitespace(char **code, char *end, Unt *linenumber) {
    char *p = *code;
    Bool found = false;
    while (p < end) {
        if (*p == ' ' || *p == '\t') {
            /* Do nothing special */
        } else if (*p == '\r' && p + 1 < end && *(p + 1) == '\f') {
            p++;
        } else if (*p == '\n' || *p == '\r' || *p == '\f') {
            (*linenumber)++;
        } else {
            if (read_munch_comment(&p, end, linenumber)) {
                continue;
            }
            break;
        }
        p++;
        found = true;
    }
    *code = p;
    return found;
}

Bool read_munch_comment(char **code, char *end, Unt *linenumber) {
    char *p = *code;
    if (p < end && *p != ';') {
        return false;
    }
    p++;
    while (p < end) {
        if (read_char_exists_in(*p, "\n\r\f")) {
            (*linenumber)++;
            break;
        }
        p++;
    }
    *code = p;
    return true;
}

Bool read_integer(char **code, char *end, Unt *linenumber, Value *result) {
    (void) linenumber; /* linenumber not actually used */
    char *p = *code;
    Bool found = false;

    if (p < end && *p == '-') {
        p++;
    }
    /* if (*p == "0" && *(++p) == 'x') { */
    /*     p++; */
    /* } */

    while (p < end && *p >= '0' && *p <= '9') {
        p++;
        found = true;
    }
    if (!found) {
        return false;
    }

    if (!read_at_delimiter(p, end)) {
        return false;
    }

    Int number;
    Int count = sscanf(*code, "%d", &number);
    if (count == EOF || count <= 0) {
        return false;
    } else {
        *result = VALUE_INTEGER(number);
        *code = p;
        return true;
    }
}

/* TODO: ensure that this works if end comes unexpected */
Bool read_float(char **code, char *end, Unt *linenumber, Value *result) {
    (void) linenumber; /* linenumber not actually used */
    char *p = *code;
    Bool found = false;
    Bool decimals = false;

    if (p < end && *p == '-') {
        p++;
    }

    if (p < end && *p == '.') {
        p++;
        decimals = true;
    }

    while (p < end && *p >= '0' && *p <= '9') {
        p++;
        found = true;
    }
    if (!found) {
        return false;
    }

    if (!decimals) {
        if (p < end && *p == '.') {
            p++;
            decimals = true;
        }
        while (p < end && *p >= '0' && *p <= '9') {
            p++;
        }
    }

    if (p < end && (*p == 'e' || *p == 'E')) {
        /* Create a "roll-back point" */
        char *p_previous = p;

        p++;

        if (p < end && (*p == '+' || *p == '-')) {
            p++;
        }

        found = false;
        while (p < end && *p >= '0' && *p <= '9') {
            p++;
            found = true;
        }
        if (!found) {
            p = p_previous;
        }

        decimals = true;
    }

    if (!read_at_delimiter(p, end)) {
        return false;
    }

    if (!decimals) {
        return false;
    }

    Double number;
    Int count = sscanf(*code, "%lf", &number);
    if (count == EOF || count <= 0) {
        return false;
    } else {
        *result = VALUE_FLOAT(number);
        *code = p;
        return true;
    }
}

Bool read_string(char **code, char *end, Unt *linenumber, Value *result) {
    (void) linenumber; /* linenumber not actually used */
    char *p = *code;

    if (p >= end || *p != '\"') {
        return false;
    }
    p++;

    while (p < end && *p != '"') {
        if (*p == '\\') {
            p++;
        }
        p++;
    }

    /* The string should end on a ", not the end  */
    if (p >= end) {
        return false;
    }
    p++;

    Unt length = (p - 1) - (*code + 1); /* substract the two surrounding " ... " */

    String *string = string_create_from_substr(*code + 1, length);
    *result = VALUE_STRING(string);
    *code = p;
    return true;
}

Bool read_symbol(char **code, char *end, Unt *linenumber, Value *result) {
    (void) linenumber; /* linenumber not actually used */
    char *p = *code;
    Bool found = false;

    /* Eat leading digits */
    while (p < end && *p >= '0' && *p <= '9') {
        p++;
    }

    while (p < end) {
        /* illegal now */
        if (read_at_delimiter(p, end) || (*p >= '0' && *p <= '9')) {
            break;
        }
        /* skip escaped */
        if (*p == '\\') {
            if (p + 1 < end) {
                p++;
            } else {
                return false;
            }
        } else {
            /* Normal character */
            p++;
        }
        /* Some character, potentially followed by a number */
        while (p < end && *p >= '0' && *p <= '9') {
            p++;
        }
        found = true;
    }

    if (!read_at_delimiter(p, end)) {
        return false;
    }
    if (!found) {
        return false;
    }

    String *symbol_name = string_create_from_substr(*code, p - *code);
    if (string_compare_str(symbol_name, "nil") == 0) {
        *result = VALUE_NIL;
        *code = p;
        return true;
    }
    if (string_compare_str(symbol_name, "error") == 0) {
        *result = VALUE_ERROR;
        *code = p;
        return true;
    }

    Value string = VALUE_STRING(symbol_name);
    Value symbol = symbol_get(string);
    *result = symbol;
    *code = p;
    return true;

}

Bool read_list(char **code, char *end, Unt *linenumber, Value *result) {
    char *p = *code;
    Unt line = *linenumber;

    if (p >= end || *p != '(') {
        return false;
    }
    p++;
    read_munch_whitespace(&p, end, &line);

    Value list = VALUE_NIL;
    while (p < end && *p != ')') {
        Value value;
        if (!read_expression(&p, end, &line, &value)) {
            /* list_destroy(list); */
            return false;
        }
        list = CONS(value, list);
        read_munch_whitespace(&p, end, &line);
    }
    list = list_reverse(list);
    if (p >= end) {
        /* list_destroy(list); */
        return false;
    }
    p++;

    *linenumber = line;
    *code = p;
    *result = list;
    return true;
}

Bool read_quote(char **code, char *end, Unt *linenumber, Value *result) {
    char *p = *code;

    if (p >= end || *p != '\'') {
        return false;
    }
    p++;
    Value value;
    if (!read_expression(&p, end, linenumber, &value)) {
        return false;
    }

    *result = CONS(symbols_quote, CONS1(value));
    *code = p;
    return true;
}

Bool read_at_delimiter(char *character, char *end) {
    /* No . */
    return character == end || !character || read_char_exists_in(*character, " \t\n\r\f()[];\"',:#");
}
Bool read_char_exists_in(char character, char* text) {
    while (*text != '\0') {
        if (character == *text) {
            return true;
        }
        text++;
    }
    return false;
}
