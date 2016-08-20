#ifndef W_HASH_H
#define W_HASH_H

#include "types.h"

#define HASH_DEFAULT_SIZE 64
#define HASH_EXPANSION_FACTOR 2
#define HASH_EXPAND_LIMIT 0.8
#define HASH_CONTRACT_LIMIT 0.2

typedef enum {
    HASH_EMPTY,
    HASH_OCCUPIED,
    HASH_DELETED,
} Hash_status;

typedef struct {
    Hash_status status;
    Unt key_hash1; /*Store the entire h1 hash*/
    Unt key_hash2; /*Store the entire h2 hash*/
    Value key;
    Value data;
} Hash_entry;

struct Hash_s {
    Unt refcount;
    Unt size;
    Unt length;
    Hash_entry *entries;
};


Hash *hash_create(void);
/* void hash_destroy(Hash *); */

void hash_set(Hash *hash, Value key, Value data);
Bool hash_get(Hash *hash, Value key, Value *result);
Bool hash_delete(Hash *hash, Value key);


/* Unt hash_length(Hash *hash); */
#define hash_length(hash) (hash -> length)

#endif
