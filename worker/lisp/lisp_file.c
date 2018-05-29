#include "../headers.h"

LISP_BUILTIN(read_file, "") {
    ENSURE_NOT_EMPTY(args);
    Value file_v = NEXT(args);
    ENSURE_EMPTY(args);
    ENSURE(file_v.type == STRING);
    FILE *file = fopen(file_v.val.string_val -> text, "r");
    if (! file) {
        return VALUE_ERROR;
    }

    Unt size = 1 << 12;
    char *buffer = NEW_BUFFER(char, size);
    Unt i = 0;
    while (true) {
        Int read = fgetc(file);
        if (read == EOF) {
            break;
        }
        if (i == size) {
            /* Resize buffer */
            Unt new_size = size * 2;
            debug("Resize: %u", new_size)
            char *new_buffer = NEW_BUFFER(char, new_size);
            debugi(size);
            debugi(i);
            debugi(new_size);
            memcpy(new_buffer, buffer, size);
            size = new_size;
            buffer = new_buffer;
        }
        buffer[i] = read;
        i++;
    }
    return VALUE_STRING(string_create_from_substr(buffer, i));
}
