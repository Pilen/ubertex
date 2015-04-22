#ifndef Z_LIST_H
#define Z_LIST_H

#include <stdlib.h>

#include "types.h"

#define LIST_EXPAND_FACTOR 2
#define LIST_CONTRACT_FACTOR 2

struct List_s {
    Unt refcount;
    Unt size;
    Unt start;
    Unt length;
    Value *data;
};


List *list_create(Unt size);
void list_destroy(List *list);
/* void list_clear(List *list); */
void list_push_front(List *list, Value value);
void list_push_back(List *list, Value value);
Value list_pop_front(List *list);
Value list_pop_back(List *list);
/* void list_insert(List *list, Value value, Unt position); */
/* Value list_pop(List* list, Unt position); */
Bool list_set(List *list, Unt position, Value value);
Value list_get(List *list, Unt position);

/* TODO: ensure that it is actually beneficial to do it like this and not just the same. */
/* List *list_create_empty(void); */
#define list_create_empty() list_create(0)
/* Unt list_length(List *list); */
#define list_length(list) (list -> length)

/* TODO: use this in list.c */
/* List *LIST_GET_UNSAFE(List *list, Unt position); */
#define LIST_GET_UNSAFE(list, position) (list) -> data[((list) -> start + (position)) % (list) -> size]

#endif
