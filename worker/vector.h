#ifndef W_VECTOR_H
#define W_VECTOR_H

#include <stdlib.h>

#define VECTOR_EXPANSION_FACTOR 2
#define VECTOR_CONTRACT_LIMIT 0.33

struct Vector_s {
    Unt refcount;
    Unt size; // In number of elements
    Unt start;
    Unt length;
    Value *data;
};


/* TODO: ensure this is called with size not length */
Vector *vector_create(Unt size);
void vector_destroy(Vector *vector);
/* void vector_clear(Vector *vector); */
void vector_push_front(Vector *vector, Value value);
void vector_push_back(Vector *vector, Value value);
Value vector_pop_front(Vector *vector);
Value vector_pop_back(Vector *vector);
/* void vector_insert(Vector *vector, Value value, Unt position); */
/* Value vector_pop(Vector* vector, Unt position); */
Bool vector_set(Vector *vector, Unt position, Value value);
Value vector_get(Vector *vector, Unt position);
void vector_normalize(Vector *vector);

/* TODO: ensure that it is actually beneficial to do it like this and not just the same. */
/* Vector *vector_create_empty(void); */
#define vector_create_empty() vector_create(0)
/* Unt vector_length(Vector *vector); */
#define vector_length(vector) (vector -> length)

/* TODO: use this in vector.c */
/* Vector *VECTOR_GET_UNSAFE(Vector *vector, Unt position); */
#define VECTOR_GET_UNSAFE(vector, position) (vector) -> data[((vector) -> start + (position)) % (vector) -> size]

void vector_sort(Vector *vector, Int (*compare)(Value a, Value b));


#endif
