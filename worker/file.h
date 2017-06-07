#ifndef W_FILE_H
#define W_FILE_H

char *file_get_extension_str(char *path);
Bool file_read_raw(char *filename, char **buffer, size_t *size);

#endif
