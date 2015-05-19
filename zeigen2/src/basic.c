#include "debug.h"

#include "basic.h"
#include "list.h"
#include "string.h"
#include "assert.h"

Bool equal(Value a, Value b) {
    /* TODO: find out how to handle comparison of 2.0 and 2 */
    if (a.type != b.type) {
        return false;
    }
    switch (a.type) {
    case ERROR:
        return true;
    case NIL:
        return true;
    case SYMBOL:
        return a.val.symbol_val == b.val.symbol_val;
    case INTEGER:
        return a.val.integer_val == b.val.integer_val;
    case FLOAT:
        return a.val.float_val == b.val.float_val;
    case STRING:
        /* Assume the strings are normalized */
        if (a.val.string_val -> size != b.val.string_val -> size) {
            return false;
        }
        /* eq */
        if (a.val.string_val == b.val.string_val) {
            return true;
        }
        Unt size = a.val.string_val -> size;
        for (Unt i = 0; i < size; i++) {
            if (a.val.string_val -> text[i] !=
                b.val.string_val -> text[i]) {
                return false;
            }
            /* NOTE: if the string can end (with '\0') before size we could end sooner */
            /* NOTE: this could be avoided if we itterated over the length, not the size */
            /* if (a.val.string_val -> text[i] == '\0') { */
            /*     return true; */
            /* } */
        }
        return true;
    case LIST:
        {
            List *a_list = a.val.list_val;
            List *b_list = b.val.list_val;
            if (a_list == b_list) {
                return true;
            }
            if (a_list -> length != b_list -> length) {
                return false;
            }
            /* Compare each element recursively */
            for (int i = 0; i < a_list -> length; i++) {
                if (!equal(LIST_GET_UNSAFE(a_list, i),
                           LIST_GET_UNSAFE(b_list, i))) {
                    return false;
                }

            }
            return true;
        }
    case HASH:
        /* TODO: implement */
        debug("HASH VALUES CANT BE COMPARED YET!!!!");
        assert(false);
    default:
        assert(false)
    }
    return false;
}

Bool eq(Value a, Value b) {
    if (a.type != b.type) {
        return false;
    }
    switch (a.type) {
    case ERROR:
        return true;
    case NIL:
        return true;
    case SYMBOL:
        return a.val.symbol_val == b.val.symbol_val;
    case INTEGER:
        return a.val.integer_val == b.val.integer_val;
    case FLOAT:
        return a.val.float_val == b.val.float_val;
    case STRING:
        return a.val.string_val == b.val.string_val;
    case LIST:
        return a.val.string_val == b.val.string_val;
    case HASH:
        return a.val.hash_val == b.val.hash_val;
    default:
        assert(false);
    }
    return false;
}

Value copy_deep(Value value) {
    switch (value.type) {
    case LIST:
        {
            List *old_list = value.val.list_val;
            List *new_list = list_create(old_list -> size);
            for (Unt i = 0; i < old_list -> length; i++) {
                Value copied = copy_deep(LIST_GET_UNSAFE(old_list, i));
                list_push_back(new_list, copied);
            }
        }
    case HASH:
    default:
        return value;
    }
}
