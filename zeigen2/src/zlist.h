#ifndef Z_LIST_H
#define Z_LIST_H

#include "ztypes.h"

#define LIST_EXPAND_FACTOR 2.0f
#define LIST_CONTRACT_FACTOR 0.5f

typedef struct {
    Unt size;
    Unt length;
    unt start;
} List;

List *list_create_empty();
List *list_create(Unt size);
List *list_destroy(List *list);
List *list_clear(List *list);
List *list_push_front(List *list, Value value);
List *list_push_back(List *list, Value value);
Value list_pop(List* list, Unt position);
Value list_pop_front(List *list);
Value list_pop_back(List *list);
Unt list_length(List *list);
Unt list_set(List *list, Unt position, Value value);
Value list_get(List *list, Unt position);

/* Private */
void list_expand(List *);
void list_contract(List *);

#endif
