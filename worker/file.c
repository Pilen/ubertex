#include <string.h>

#include "headers.h"

char *file_get_extension_str(char *filename) {
    char *extension = strrchr(filename, '.');
    if (!extension || extension == filename) {
        return "";
    } else {
        return extension + 1;
    }
}

Bool file_read_raw(char *filename, char **buffer, size_t *size) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        log_error("Could not open file %s", filename);
        return false;
    }
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    w_assert(file_size > 0);
    rewind(file);

    char *allocated = memory_malloc(sizeof(char) * (file_size + 1));
    size_t bytes_read = fread(allocated, sizeof(char), file_size, file);
    if (bytes_read != (size_t) file_size) {
        log_error("Could not read entire file %s", filename);
        fclose(file);
        memory_free(allocated);
        return false;
    }
    allocated[file_size] = '\0';
    *buffer = allocated;
    *size = file_size;
    return true;
}
