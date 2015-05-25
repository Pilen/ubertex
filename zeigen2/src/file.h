#ifndef Z_FILE_H
#define Z_FILE_H

#include "types.h"

char *file_get_extension_str(char *path);
Bool file_read_raw(char *filename, char **buffer, size_t *size);

#endif
