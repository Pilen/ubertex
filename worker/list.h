#ifndef W_LIST_H
#define W_LIST_H

struct Cons_s {
    Value car;
    Value cdr;
};

Cons *list_cons(Value a, Value b);
#define list_cons1(value) list_cons(value, VALUE_NIL)
Value list_length(Value list);
Value list_nth(Value list, Unt n);
void list_destroy(Value list);
Value list_reverse(Value list); /* Inplace. Remember always to assign the returned value (do not rely only on modification) */
Value list_copy(Value list);

#endif
