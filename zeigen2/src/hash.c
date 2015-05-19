#include "debug.h"
#include "print.h"
#include "string.h"
#include "hash.h"
#include "memory.h"
#include "basic.h"
#include "assert.h"

void hash_insert(Hash *hash, Unt h1, Unt h2, Value key, Value data);
Unt hash_calculate1(const Value *key);
Unt hash_calculate2(const Value *key);
Unt hash_index(Unt h1, Unt h2, Unt i, Unt size);
void hash_resize(Hash *hash);


Hash *hash_create(void) {
    assert_build((1.0 / HASH_EXPANSION_FACTOR) > HASH_CONTRACT_LIMIT);
    assert_build(HASH_EXPAND_LIMIT > HASH_CONTRACT_LIMIT);
    assert_build(HASH_EXPAND_LIMIT <= 1.0);
    Hash *hash = memory_malloc(sizeof(Hash));
    Hash_entry *entries = memory_calloc(HASH_DEFAULT_SIZE,
                                   sizeof(Hash_entry));

    hash -> refcount = 0;
    hash -> size = HASH_DEFAULT_SIZE;
    hash -> length = 0;
    hash -> entries = entries;

    return hash;
}

void hash_destroy(Hash *);

void hash_set(Hash *hash, Value key, Value data) {
    /* Could also be done by iterating over the entries,
       remembering the first occurrence of a deleted entry.
       if a matching key is found, it is deleted and iteration is stopped.
       when iteration stops we see if there were any deleted and insert
       the new entry there else we insert it here. */


    /* Ensure that the old key/value pair does not persist */
    hash_delete(hash, key);

    /* Resize if necessary */
    hash_resize(hash);

    Unt h1 = hash_calculate1(&key);
    Unt h2 = hash_calculate2(&key);
    hash_insert(hash, h1, h2, key, data);
}

Bool hash_get(Hash *hash, Value key, Value *result) {
    Unt size = hash -> size;
    Unt h1 = hash_calculate1(&key);
    Unt h2 = hash_calculate2(&key);

    for (Unt i = 0; i < size; i++) {
        Unt index = hash_index(h1, h2, i, size);
        Hash_entry *entry = hash -> entries + index;

        if (entry -> status == HASH_EMPTY) {
            *result = VALUE_NIL;
            return false;
        }
        /* if (entry -> status == HASH_DELETED) { */
        /*     And found later, relocate it to first deleted entry */
        /* } */
        if (entry -> status == HASH_OCCUPIED) {
            if (entry -> key_hash1 == h1 &&
                entry -> key_hash2 == h2 &&
                equal(entry -> key, key)) {
                *result = entry -> data;
                return true;
            }
        }
    }

    /* TODO: should it return nil or some other value? */
    *result = VALUE_NIL;
    return false;
}

Bool hash_delete(Hash *hash, Value key) {
    Unt size = hash -> size;
    Unt h1 = hash_calculate1(&key);
    Unt h2 = hash_calculate2(&key);

    for (Unt i = 0; i < size; i++) {
        Unt index = hash_index(h1, h2, i, size);
        Hash_entry *entry = hash -> entries + index;

        if (entry -> status == HASH_EMPTY) {
            return false;
        }

        if (entry -> status == HASH_OCCUPIED) {
            if (entry -> key_hash1 == h1 &&
                entry -> key_hash2 == h2 &&
                equal(entry -> key, key)) {
                memory_ref_dec(entry -> key);
                memory_ref_dec(entry -> data);
                entry -> status = HASH_DELETED;
                hash -> length--;

                hash_resize(hash);
                return true;
            }
        }
    }
    return false;
}

void hash_insert(Hash *hash, Unt h1, Unt h2, Value key, Value data) {
    Unt size = hash -> size;

    /* At least one entry is guaranteed to be empty */
    for (Unt i = 0; i < size; i++) {
        Unt index = hash_index(h1, h2, i, size);
        Hash_entry *entry = hash -> entries + index;
        if (entry -> status == HASH_EMPTY || entry -> status == HASH_DELETED) {

            entry -> status = HASH_OCCUPIED;
            entry -> key_hash1 = h1;
            entry -> key_hash2 = h2;
            /* NOTE: Should a copy of key be stored instead? */
            entry -> key = key;
            entry -> data = data;
            memory_ref_inc(key);
            memory_ref_inc(data);

            hash -> length++;
            return;
        }
    }
    assert(false);
}
Unt hash_calculate1(const Value *key) {
    /* TODO: implement properly!! */
    return 1;
}
Unt hash_calculate2(const Value *key) {
    /* TODO: implement properly!! */
    return 1;
}
Unt hash_index(Unt h1, Unt h2, Unt i, Unt size) {
    /* TODO: assert(h2 != 0); */
    /* Uses double hashing */
    /* If h2 == 1, then it is equivalent to linear hashing */
    return (h1 + i * h2) % size;
}

void hash_resize(Hash *hash) {
    Unt old_size = hash -> size;
    Hash_entry *old_entries = hash -> entries;

    Unt new_size;
    if (hash -> length >= old_size * HASH_EXPAND_LIMIT) {
        new_size = old_size * HASH_EXPANSION_FACTOR;
    } else if (hash -> length < old_size * HASH_CONTRACT_LIMIT) {
        new_size = old_size / HASH_EXPANSION_FACTOR;
    } else {
        return;
    }
    if (new_size <= HASH_DEFAULT_SIZE) {
        return;
    }
    Hash_entry *new_entries = memory_calloc(new_size, sizeof(Hash_entry));

    hash -> size = new_size;
    hash -> entries = new_entries;
    hash -> length = 0; /* This is updated when each entry is inserted */

    for (Unt i = 0; i < old_size; i++) {
        Hash_entry *entry = old_entries + i;
        if (entry -> status == HASH_OCCUPIED) {
            Unt h1 = entry -> key_hash1;
            Unt h2 = entry -> key_hash2;
            Value key = entry -> key;
            Value data = entry -> data;
            hash_insert(hash, h1, h2, key, data);
        }
    }
}
