#include "debug.h"
#include "vector.h"
#include "memory.h"
#include "assert.h"

void vector_expand(Vector *vector);
void vector_contract(Vector *vector);



Vector *vector_create(Unt size) {
    assert_build((1.0 / VECTOR_EXPANSION_FACTOR) > VECTOR_CONTRACT_LIMIT);
    Vector *vector = memory_malloc(sizeof(Vector));
    Value *data= memory_cmalloc(sizeof(Value) * size);

    vector -> refcount = 0;
    vector -> size = size;
    vector -> start = 0;
    vector -> length = 0;
    vector -> data = data;

    return vector;
}

void vector_destroy(Vector *vector) {
}

/* void vector_clear(Vector *vector); */

void vector_push_front(Vector *vector, Value value) {
    if (vector -> length == vector -> size) {
        vector_expand(vector);
    }

    Unt index = (vector -> start - 1) % vector -> size;
    vector -> data[index] = value;
    vector -> start = index;
    vector -> length++;

    memory_ref_inc(value);
}

void vector_push_back(Vector *vector, Value value) {
    if (vector -> length == vector -> size) {
        vector_expand(vector);
    }

    Unt index = (vector -> start + vector -> length) % vector -> size;
    vector -> data[index] = value;
    vector -> length++;

    memory_ref_inc(value);
}

Value vector_pop_front(Vector *vector) {
    if (vector -> size <= 0) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    Value value = VECTOR_GET_UNSAFE(vector, 0);
    memory_ref_dec(value);

    /* Move start 1 forward and decrease length so end stays the same */
    vector -> start = (vector -> start + 1) % vector -> size;
    vector -> length--;

    /* TODO: ensure correctness */
    if (vector -> length < vector -> size * VECTOR_CONTRACT_LIMIT) {
        vector_contract(vector);
    }

    return value;
}

Value vector_pop_back(Vector *vector) {
    if (vector -> size <= 0) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    Value value = VECTOR_GET_UNSAFE(vector, vector -> length - 1);
    memory_ref_dec(value);

    vector -> length--;

    /* TODO: ensure correctness */
    if (vector -> length < vector -> size * VECTOR_CONTRACT_LIMIT) {
        vector_contract(vector);
    }

    return value;
}

/* void vector_insert(Vector *vector, Value value, Unt position) */

Value vector_pop(Vector* vector, Unt position) {
    if (position < 0) {
        position = vector -> length + position;
    }
    if (position >= vector -> length) {
        /* TODO: log error */
        return VALUE_ERROR;
    }

    if (position == 0) {
        return vector_pop_front(vector);
    }
    if (position == vector -> length - 1) {
        return vector_pop_back(vector);
    }

    /*
      [XXX-XXX]
      3
    */

    /* TODO: finish function */
    return VALUE_ERROR;
}


Bool vector_set(Vector *vector, Unt position, Value value) {
    if (position < 0) {
        position = vector -> length + position;
    }
    if (position >= vector -> length) {
        /* TODO: log error */
        return false;
    }

    Unt index = (vector -> start + position) % vector -> size;

    memory_ref_dec(vector -> data[index]);
    memory_ref_inc(value);

    vector -> data[index] = value;
    return true;
}

Value vector_get(Vector *vector, Unt position) {
    if (position < 0) {
        position = vector -> length + position;
    }
    if (position >= vector -> length) {
        /* TODO: log error */
        return VALUE_ERROR;
    } /* else if (position < -(vector -> length)) {/\* TODO: log error *\/ return VALUE_ERROR;} */

    return VECTOR_GET_UNSAFE(vector, position);
}

void vector_normalize(Vector *vector) {
    if (vector -> start + vector -> length <= vector -> size) {
        return;
    }
    Value *new_data = memory_cmalloc(sizeof(Value) * vector -> size);
    Unt new_start = (vector -> size - vector -> length) / 2;
    for (Unt i = 0; i < vector -> length; i++) {
        Unt index = (new_start + i ) % vector -> size;
        new_data[index] = VECTOR_GET_UNSAFE(vector, i);
    }
    vector -> start = new_start;
    vector -> data = new_data;
}

/**** Private ****/
void vector_expand(Vector *vector) {
    Unt new_size = vector -> size * VECTOR_EXPANSION_FACTOR;
    /* Ensure the vector always expands by at least 1 */
    if (new_size == vector -> size) {
        new_size++;
    }
    Value *new_data = memory_cmalloc(sizeof(Value) * new_size);

    /* TODO: ensure correctenes of castings + roundings
           is start at the correct place when start = 0 and start at end? */
    Unt new_start;
    if (vector -> size > 0) {
        new_start = (Unt) ((float) vector -> start / vector -> size) * new_size;
    } else {
        new_start = 0;
    }
    Value *new_value = new_data + new_start;

    for (Unt i = 0; i < vector -> length; i++) {
        *new_value = VECTOR_GET_UNSAFE(vector, i);
        new_value = new_data + ((1 + new_value - new_data) % new_size);
    }

    vector -> size = new_size;
    vector -> start = new_start;
    vector -> data = new_data;
}

void vector_contract(Vector *vector) {
    /* TODO, design a "contract" for when to call vector_contract,
       so that the content will always fit! (remember division floors)
       It might be a good idea with an assertion here!

       Something along, expansion and contraction is always a multiple of VECTOR_EXPANSION_FACTOR.
       contraction happens when the length is below the size * VECTOR_CONTRACT_LIMIT

 */

    /* contract to previous size when under limit */
    Unt new_size = vector -> size / VECTOR_EXPANSION_FACTOR;
    Value *new_data = memory_cmalloc(sizeof(Value) * new_size);
    /* TODO: ensure correctenes of castings + roundings
       is start at the correct place when start = 0 and start at end? */
    Unt new_start = (Unt) ((float) vector -> start / vector -> size) * new_size;
    Value *new_value = new_data + new_start;

    for (Unt i = 0; i < vector -> length; i++) {
        *new_value = VECTOR_GET_UNSAFE(vector, i);
        new_value = new_data + ((1 + new_value - new_data) % new_size);

    }

    vector -> size = new_size;
    vector -> start = new_start;
    vector -> data = new_data;
}

void vector_sort(Vector *vector, Int (*compare)(Value a, Value b)) {
    /* NOTE: Look into how this can be integrated with the resource sorting */

    /* if (vector -> start + vector -> length >= vector -> size) { */
    /*     /\* TODO: implement *\/ */
    /*     /\* vector_normalize(vector); *\/ */
    /*     w_assert(false); */
    /* } */

    /* qsort(vector -> data, vector -> length, sizeof(Value), vector_sort_comparison, compare); */
}

void vector_sort_comparison(const void *a, const void*b, void *comparison) {
    /* return comparison(* (Value *)a, * (Value *) b); */
}
