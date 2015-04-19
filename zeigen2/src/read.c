#include "debug.h"

#include <stdio.h>
#include <string.h>

#include "ztypes.h"
#include "zmemory.h"
#include "zread.h"
#include "zsymbol.h"

#include "zstring.h"

/* TODO: this file does not work with unicode yet!!! */

Value read_expression(char **code, char *end, Unt *linenumber);
Bool read_munch_whitespace(char **code, char *end, Unt *linenumber);
Bool read_integer(char **code, char *end, Unt *linenumber, Value *result);
Bool read_float(char **code, char *end, Unt *linenumber, Value *result);
Bool read_string(char **code, char *end, Unt *linenumber, Value *result);
Bool read_symbol(char **code, char *end, Unt *linenumber, Value *result);
Bool read_char_exists_in(char character, char* text);


Value read(Value value) {
    if (value.type != STRING) {
        return VALUE_ERROR;
    }

    char *code = value.val.string_val -> text;
    char *end = code + strlen(code) + 1;
    Unt linenumber = 0;
    Value result = read_expression(&code, end, &linenumber);
    if (code == end) {
        return result;
    }
    /* TODO: log error */
    return VALUE_ERROR;
}
Value read_from_str(char *str) {
    char *code = str;
    char *end = code + strlen(code);
    /* debugv("code = %p, end = %p, diff=%td", code, end, end-code); */
    Unt linenumber = 0;
    Value result = read_expression(&code, end, &linenumber);
    if (code == end) {
        return result;
    }
    /* TODO: log error */
    return VALUE_ERROR;
}


/* End point to char just beyond the last printable, aka nul */
Value read_expression(char **code, char *end, Unt *linenumber) {
    /* Skip spaces */
    read_munch_whitespace(code, end, linenumber);

    Value result;
    if (read_float(code, end, linenumber, &result)) {
        return result;
    }
    if (read_integer(code, end, linenumber, &result)) {
        return result;
    }
    if (read_string(code, end, linenumber, &result)) {
        return result;
    }
    if (read_symbol(code, end, linenumber, &result)) {
        return result;
    }


    return VALUE_ERROR;
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
            break;
        }
        p++;
        found = true;
    }
    *code = p;
    return found;
}

Bool read_integer(char **code, char *end, Unt *linenumber, Value *result) {
    char *p = *code;
    Bool found = false;

    if (*p == '-') {
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

Bool read_float(char **code, char *end, Unt *linenumber, Value *result) {
    char *p = *code;
    Bool found = false;
    Bool decimals = false;

    if (*p == '-') {
        p++;
    }

    if (*p == '.') {
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
        if (*p == '.') {
            p++;
            decimals = true;
        }
        while (p < end && *p >= '0' && *p <= '9') {
            p++;
        }
    }

    if (*p == 'e' || *p == 'E') {
        /* Create a "roll-back point" */
        char *p_previous = p;

        p++;

        if (*p == '+' || *p == '-') {
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
    char *p = *code;

    if (*p != '\"') {
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
    char *p = *code;
    Bool found = false;

    while (p < end && *p >= '0' && *p <= '9') {
        p++;
    }

    while (p < end) {
        if (read_char_exists_in(*p, " \t\n\r\f0123456789()[];\"'.,:#\\")) {
            return false;
        }
        if (*p == '\\') {
            if (p + 1 < end) {
                p++;
            } else {
                return false;
            }
        } else {
            /* Normal character, potentially followed by a number */
            while (p < end && *p >= '0' && *p <= '9') {
                p++;
            }
        }
        p++;
        found = true;
    }

    if (!found) {
        return false;
    }


    String *symbol_name = string_create_from_substr(*code, p - *code);
    Value string = VALUE_STRING(symbol_name);
    Value symbol = symbol_add(string);
    *result = symbol;
    *code = p;
    return true;

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
