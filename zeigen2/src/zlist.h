#ifndef Z_LIST_H
#define Z_LIST_H

#include <stdlib.h>

#include "ztypes.h"

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
/* void list_destroy(List *list); */
/* void list_clear(List *list); */
void list_push_front(List *list, Value value);
void list_push_back(List *list, Value value);
/* void list_insert(List *list, Value value, Unt position); */
/* Value list_pop(List* list, Unt position); */
Value list_pop_front(List *list);
Value list_pop_back(List *list);
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


/* #define FOREACH(pointer, list)                                          \ */
/*     for (Value *pointer = (list) -> data + (list) -> start;                 \ */
/*          pointer != (list) -> data + (((list) -> start + (list) -> length) % (list) -> size); \ */
/*          pointer = (list) -> data + (((pointer - (list) -> data) + 1) % (list) -> size)) */

#define FOREACH(pointer, list)                                          \
    Value *_Z_FOREACH_END_FOR##pointer = (list) -> data + (((list) -> start + (list) -> length) % (list) -> size); \
    for (Value *pointer = (list) -> data + (list) -> start;                 \
         pointer != _Z_FOREACH_END_FOR##pointer;                        \
         pointer = (list) -> data + ((1 + pointer - (list) -> data) % (list) -> size))


/* #define FOREACH_BEGIN(pointer, list)                                    \ */
/*     {                                                                   \ */
/*         Unt _Z_FOREACH_LENGTH_FOR##pointer = (list) -> length;          \ */
/*         for (Unt _Z_FOREACH_INDEX_FOR##pointer = 0;                     \ */
/*              _Z_FOREACH_INDEX_FOR##pointer < _z_FOREACH_LENGTH_FOR##pointer; \ */
/*              _Z_FOREACH_INDEX_FOR##pointer++) {                         \ */
/*             Value *pointer = (list) -> data + (((list) -> start + _z_FOREACH_INDEX_FOR##pointer) % (list) -> size); \ */

/* #define FOREACH_END }} */

#endif
