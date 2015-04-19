#include "debug.h"

#include "zhash.h"
#include "zmemory.h"
#include "zbasic.h"

Unt hash_calculate(const Value *key);
Unt hash_linear(Unt h1, Unt i);
Unt hash_double(Unt h1, Unt h2, Unt i);
void hash_resize(Hash *hash);


Hash *hash_create(void) {
    Hash *hash = z_malloc(sizeof(Hash));
    Hash_entry *entries = z_calloc(HASH_DEFAULT_SIZE,
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

    Unt length = hash -> length;
    Unt h1 = hash_calculate(&key);
    Unt h2 = h1;

    /* At least one entry is guaranteed to be empty */
    for (Unt i = 0; i < length; i++) {
        Unt index = hash_linear(h1, i);
        Hash_entry *entry = hash -> entries + index;
        if (entry -> status == HASH_FREE || entry -> status == HASH_DELETED) {

            entry -> status = HASH_OCCUPIED;
            entry -> key_hash1 = h1;
            entry -> key_hash2 = h2;
            /* NOTE: Should a copy of key be stored instead? */
            entry -> key = key;
            entry -> data = data;
            z_ref_inc(key);
            z_ref_inc(data);

            hash -> length++;
            return;
        }
    }
}

Value hash_get(Hash *hash, Value key) {
    debugv("hash = %p", (void *)hash);
    Unt length = hash -> length;
    Unt h1 = hash_calculate(&key);
    Unt h2 = h1;

    debugv("length = %d", length);
    for (Unt i = 0; i < length; i++) {
        Unt index = hash_linear(h1, i);
        Hash_entry *entry = hash -> entries + index;

        if (entry -> status == HASH_FREE) {
            return VALUE_NIL;
        }
        /* if (entry -> status == HASH_DELETED) { */
        /*     And found later, relocate it to first deleted entry */
        /* } */
        if (entry -> status == HASH_OCCUPIED) {
            if (entry -> key_hash1 == h1 &&
                entry -> key_hash2 == h2 &&
                equal(entry -> key, key)) {
                return entry -> data;
            }
        }
    }

    /* TODO: should it return nil or some other value? */
    return VALUE_NIL;
}

Bool hash_delete(Hash *hash, Value key) {
    Unt length = hash -> length;
    Unt h1 = hash_calculate(&key);
    Unt h2 = h1;

    for (Unt i = 0; i < length; i++) {
        Unt index = hash_linear(h1, i);
        Hash_entry *entry = hash -> entries + index;

        if (entry -> status == HASH_FREE) {
            return false;
        }

        if (entry -> status == HASH_OCCUPIED) {
            if (entry -> key_hash1 == h1 &&
                entry -> key_hash2 == h2 &&
                equal(entry -> key, key)) {
                z_ref_dec(entry -> key);
                z_ref_dec(entry -> data);
                entry -> status = HASH_DELETED;
                hash -> length--;
                return true;
            }
        }
    }
    return false;
}

Unt hash_calculate(const Value *key) {
    /* TODO: implement properly!! */
    return 1;
}
Unt hash_linear(Unt h1, Unt i) {
    return h1 + i;
}
Unt hash_double(Unt h1, Unt h2, Unt i) {
    return (h1 + i * h2);
}


void hash_resize(Hash *hash) {

}
