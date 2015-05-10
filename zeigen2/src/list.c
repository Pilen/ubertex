#include "debug.h"
#include "list.h"
#include "memory.h"
#include "assert.h"

void list_expand(List *list);
void list_contract(List *list);



List *list_create(Unt size) {
    assert_build((1.0 / LIST_EXPANSION_FACTOR) > LIST_CONTRACT_LIMIT);
    List *list = memory_malloc(sizeof(List));
    Value *data= memory_calloc(size, sizeof(Value));

    list -> refcount = 0;
    list -> size = size;
    list -> start = 0;
    list -> length = 0;
    list -> data = data;

    return list;
}

void list_destroy(List *list) {
}

/* void list_clear(List *list); */

void list_push_front(List *list, Value value) {
    if (list -> length == list -> size) {
        list_expand(list);
    }

    Unt index = (list -> start - 1) % list -> size;
    list -> data[index] = value;
    list -> start = index;
    list -> length++;

    memory_ref_inc(value);
}

void list_push_back(List *list, Value value) {
    if (list -> length == list -> size) {
        list_expand(list);
    }

    Unt index = (list -> start + list -> length) % list -> size;
    list -> data[index] = value;
    list -> length++;

    memory_ref_inc(value);
}

Value list_pop_front(List *list) {
    if (list -> size <= 0) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    Value value = LIST_GET_UNSAFE(list, 0);
    memory_ref_dec(value);

    /* Move start 1 forward and decrease length so end stays the same */
    list -> start = (list -> start + 1) % list -> size;
    list -> length--;

    /* TODO: ensure correctness */
    if (list -> length < list -> size * LIST_CONTRACT_LIMIT) {
        list_contract(list);
    }

    return value;
}

Value list_pop_back(List *list) {
    if (list -> size <= 0) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    Value value = LIST_GET_UNSAFE(list, list -> length - 1);
    memory_ref_dec(value);

    list -> length--;

    /* TODO: ensure correctness */
    if (list -> length < list -> size * LIST_CONTRACT_LIMIT) {
        list_contract(list);
    }

    return value;
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


Bool list_set(List *list, Unt position, Value value) {
    if (position < 0) {
        position = list -> length + position;
    }
    if (position >= list -> length) {
        /* TODO: log error */
        return false;
    }

    Unt index = (list -> start + position) % list -> size;

    memory_ref_dec(list -> data[index]);
    memory_ref_inc(value);

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

    return LIST_GET_UNSAFE(list, position);
}

/**** Private ****/
void list_expand(List *list) {
    Unt new_size = list -> size * LIST_EXPANSION_FACTOR;
    /* Ensure the list always expands by at least 1 */
    if (new_size == list -> size) {
        new_size++;
    }
    Value *new_data= memory_calloc(new_size, sizeof(Value));

    /* TODO: ensure correctenes of castings + roundings
           is start at the correct place when start = 0 and start at end? */
    Unt new_start;
    if (list -> size > 0) {
        new_start = (Unt) ((float) list -> start / list -> size) * new_size;
    } else {
        new_start = 0;
    }
    Value *new_value = new_data + new_start;

    for (Unt i = 0; i < list -> length; i++) {
        *new_value = LIST_GET_UNSAFE(list, i);
        new_value = new_data + ((1 + new_value - new_data) % new_size);

    }

    list -> size = new_size;
    list -> start = new_start;
    list -> data = new_data;
}

void list_contract(List *list) {
    /* TODO, design a "contract" for when to call list_contract,
       so that the content will always fit! (remember division floors)
       It might be a good idea with an assertion here!

       Something along, expansion and contraction is always a multiple of LIST_EXPANSION_FACTOR.
       contraction happens when the length is below the size * LIST_CONTRACT_LIMIT

 */

    /* contract to previous size when under limit */
    Unt new_size = list -> size / LIST_EXPANSION_FACTOR;
    Value *new_data = memory_calloc(new_size, sizeof(Value));
    /* TODO: ensure correctenes of castings + roundings
       is start at the correct place when start = 0 and start at end? */
    Unt new_start = (Unt) ((float) list -> start / list -> size) * new_size;
    Value *new_value = new_data + new_start;

    for (Unt i = 0; i < list -> length; i++) {
        *new_value = LIST_GET_UNSAFE(list, i);
        new_value = new_data + ((1 + new_value - new_data) % new_size);

    }

    list -> size = new_size;
    list -> start = new_start;
    list -> data = new_data;
}
