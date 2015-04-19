
#include "zbasic.h"
#include "zlist.h"

Bool equal(Value a, Value b) {
    /* TODO: find out how to handle comparison of 2.0 and 2 */
    if (a.type != b.type) {
        return false;
    }
    switch (a.type) {
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
    default:
        return false;
    }
    return false;
}

/* Bool eq(Value a, Value b); */
