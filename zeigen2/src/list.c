
#include "zlist.h"
#include "zmemory.h"

void list_expand(List *list);
void list_contract(List *list);



List *list_create(Unt size) {
    List *list = z_malloc(sizeof(List));
    Value *data= z_calloc(size, sizeof(Value));

    list -> refcount = 0;
    list -> size = size;
    list -> start = 0;
    list -> length = 0;
    list -> data = data;

    return list;
}

/* void list_clear(List *list); */

void list_push_front(List *list, Value value) {
    if (list -> size == list -> length) {
        list_expand(list);
    }

    Unt index = list -> size - (list -> start % list -> size) -1;
    list -> data[index] = value;

    z_ref_inc(value);
}
void list_push_back(List *list, Value value) {
    if (list -> size == list -> length) {
        list_expand(list);
    }

    Unt index = (list -> start + list -> length) % list -> size;
    list -> data[index] = value;

    z_ref_inc(value);
}

/* void list_insert(List *list, Value value, Unt position) */

Value list_pop(List* list, Unt position) {
    if (position < 0) {
        position = list -> length + position;
    }
    if (position >= list -> length) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    if (position == 0) {
        return list_pop_front(list);
    }
    if (position == list -> length - 1) {
        return list_pop_back(list);
    }

    /*
    [XXX-XXX]
        3
    */

    /* TODO: finish function */
    return VALUE_ERROR;
}
Value list_pop_front(List *list) {
    if (list -> size <= 0) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    Value value = list -> data[list -> start];
    z_ref_dec(value);

    /* Move start 1 forward and decrease length so end stays the same */
    list -> start = (list -> start + 1) % list -> size;
    list -> length--;

    /* TODO: ensure correctness */
    if (list -> length < list -> size / LIST_CONTRACT_FACTOR) {
        list_contract(list);
    }

    return value;
}

Value list_pop_back(List *list) {
    if (list -> size <= 0) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    Value value = list -> data[(list -> start + list -> length -1) % list -> size];
    z_ref_dec(value);

    list -> length--;

    /* TODO: ensure correctness */
    if (list -> length < list -> size / LIST_CONTRACT_FACTOR) {
        list_contract(list);
    }

    return value;
}

Bool list_set(List *list, Unt position, Value value) {
    if (position < 0) {
        position = list -> length + position;
    }
    if (position >= list -> length) {
        /* TODO: log error */
        return false;
    }

    Unt index = (list -> start + position) % list -> size;

    z_ref_dec(list -> data[index]);
    z_ref_inc(value);

    list -> data[index] = value;
    return true;
}

Value list_get(List *list, Unt position) {
    if (position < 0) {
        position = list -> length + position;
    }
    if (position >= list -> length) {
        /* TODO: log error */
        return VALUE_ERROR;
    } /* else if (position < -(list -> length)) {/\* TODO: log error *\/ return VALUE_ERROR;} */

    Unt index = (list -> start + position) % list -> size;
    return list -> data[index];
}

/**** Private ****/
void list_expand(List *list) {
    Unt new_size = list -> size * LIST_EXPAND_FACTOR;
    /* Ensure the list always expands by at least 1 */
    if (new_size == list -> size) {
        new_size++;
    }

    Value *new_data= z_calloc(new_size, sizeof(Value));

    /* TODO: ensure correctenes of castings + roundings
           is start at the correct place when start = 0 and start at end? */
    Unt new_start = (Unt) ((float) list -> start / list -> size) * new_size;
    Value *new_value = new_data + new_start;

    FOREACH(old_value, list) {
        *new_value = *old_value;
        new_value = new_data + ((1 + new_value - new_data) % new_size);
    }

    list -> size = new_size;
    list -> start = new_start;
    list -> data = new_data;
}

void list_contract(List *list) {
    /* TODO, design a "contract" for when to call list_contract,
       so that the content will always fit! (remember division floors)
       It might be a good idea with an assertion here!*/
    Unt new_size = list -> size / LIST_CONTRACT_FACTOR;
    Value *new_data = z_calloc(new_size, sizeof(Value));
    /* TODO: ensure correctenes of castings + roundings
       is start at the correct place when start = 0 and start at end? */
    Unt new_start = (Unt) ((float) list -> start / list -> size) * new_size;
    Value *new_value = new_data + new_start;

    FOREACH(old_value, list) {
        *new_value = *old_value;
        new_value = new_data + ((1 + new_value - new_data) % new_size);
    }

    list -> size = new_size;
    list -> start = new_start;
    list -> data = new_data;
}
