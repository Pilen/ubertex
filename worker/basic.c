#include "debug.h"

#include "basic.h"
#include "list.h"
#include "vector.h"
#include "string.h"
#include "assert.h"
#include "image.h"
#include "pdf.h"
#include "sound.h"
#include "text.h"
#include "lisp.h"

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
    case CONS:
        if (!equal(CAR(a), CAR(b))) {
            return false;
        }
        return equal(CDR(a), CDR(b));
    case VECTOR:
        {
            Vector *a_vector = a.val.vector_val;
            Vector *b_vector = b.val.vector_val;
            if (a_vector == b_vector) {
                return true;
            }
            if (a_vector -> length != b_vector -> length) {
                return false;
            }
            /* Compare each element recursively */
            for (int i = 0; i < a_vector -> length; i++) {
                if (!equal(VECTOR_GET_UNSAFE(a_vector, i),
                           VECTOR_GET_UNSAFE(b_vector, i))) {
                    return false;
                }

            }
            return true;
        }
    case HASH:
        /* TODO: implement */
        debug("HASH VALUES CANT BE COMPARED YET!!!!");
        w_assert(false);
    case LAMBDA:
        {
            Lambda *a_l = a.val.lambda_val;
            Lambda *b_l = b.val.lambda_val;
            return (equal(a_l -> parameters, b_l -> parameters) &&
                    equal(a_l -> body, b_l -> body) &&
                    equal(a_l -> lexical, b_l -> lexical));
        }
    case IMAGE:
        return equal(a.val.image_val -> path, b.val.image_val -> path);
    case PDF:
        return equal(a.val.pdf_val -> path, b.val.pdf_val -> path);
    case SOUNDSAMPLE:
        return ((a.val.soundsample_val -> dirty || b.val.soundsample_val -> dirty)
                || equal(a.val.soundsample_val -> path, b.val.soundsample_val -> path));
    default:
        w_assert(false)
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
        return a.val.vector_val == b.val.vector_val;
    case CONS:
        return a.val.cons_val == b.val.cons_val;
    case VECTOR:
        return a.val.string_val == b.val.string_val;
    case HASH:
        return a.val.hash_val == b.val.hash_val;
    case LAMBDA:
        return a.val.lambda_val == b.val.lambda_val;
    default:
        w_assert(false);
    }
    return false;
}

Value copy_deep(Value value) {
    //TODO: implement
    switch (value.type) {
    case CONS:
        return CONS(copy_deep(CAR(value)), copy_deep(CDR(value)));
    case VECTOR:
        {
            Vector *old_vector = value.val.vector_val;
            Vector *new_vector = vector_create(old_vector -> size);
            for (Unt i = 0; i < old_vector -> length; i++) {
                Value copied = copy_deep(VECTOR_GET_UNSAFE(old_vector, i));
                vector_push_back(new_vector, copied);
            }
            return VALUE_VECTOR(new_vector);
        }
        /* Immutable */
    case ERROR:
    case NIL:
    case SYMBOL:
    case INTEGER:
    case FLOAT:
    case STRING:
        return value;
    case HASH:
    case LAMBDA: // Should copy lexicals into a new lambda
        log_error("Not yet implemented");
    default:
        w_assert(false);
        return value;
    }
}
