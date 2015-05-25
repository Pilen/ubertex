#include <string.h>
#include "file.h"

char *file_get_extension_str(char *filename) {
    char *extension = strrchr(filename, '.');
    if (!extension || extension == filename) {
        return "";
    } else {
        return extension + 1;
    }
}
