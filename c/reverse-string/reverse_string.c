#include "reverse_string.h"

#include <stdlib.h>
#include <string.h>

char *reverse(const char *value)
{
    size_t len = strlen(value);
    char *rev = malloc((len + 1) * sizeof(char));
    char *p = rev + len;
    for (*p-- = '\0'; *value != '\0'; ++value, --p) {
        *p = *value;
    }

    return rev;
}
