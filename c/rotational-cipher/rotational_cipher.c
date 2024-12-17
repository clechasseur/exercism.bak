#include "rotational_cipher.h"

#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

static bool rotate_alpha(char *out, char c, char a, char z, int shift_key)
{
    bool rotated = c >= a && c <= z;
    if (rotated) {
        *out = a + (c - a + shift_key) % 26;
    }
    return rotated;
}

char *rotate(const char *text, int shift_key)
{
    size_t text_len = strlen(text);
    char *rotated = malloc(text_len + 1);

    char *out = rotated;
    for (; *text != '\0'; ++text, ++out) {
        if (!rotate_alpha(out, *text, 'A', 'Z', shift_key) && !rotate_alpha(out, *text, 'a', 'z', shift_key)) {
            *out = *text;
        }
    }
    *out = '\0';

    return rotated;
}
