#include "run_length_encoding.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *encode_char(char *dest, char c, unsigned int count)
{
    if (count > 1) {
        dest += sprintf(dest, "%u", count);
    }
    *dest++ = c;
    return dest;
}

char *encode(const char *text)
{
    size_t text_len = strlen(text);
    char *encoded = malloc(text_len + 1);
    if (text_len == 0) {
        *encoded = '\0';
        return encoded;
    }

    char *dest = encoded;
    char prev = *text++;
    unsigned int count = 1;
    for (; *text != '\0'; ++text) {
        if (*text == prev) {
            ++count;
        } else {
            dest = encode_char(dest, prev, count);
            prev = *text;
            count = 1;
        }
    }
    dest = encode_char(dest, prev, count);
    *dest = '\0';

    return encoded;
}

static int is_digit(char c)
{
    return c >= '0' && c <= '9';
}

static unsigned int digit_value(char c)
{
    return c - '0';
}

static void expand_dest(char **dest, size_t *dest_size, size_t data_len, size_t used, unsigned int needed)
{
    while ((*dest_size - used - 1) < needed) {
        *dest_size += data_len;
        *dest = realloc(*dest, *dest_size);
    }
}

static void decode_char(char *dest, char c, unsigned int count)
{
    while (count-- != 0) {
        *dest++ = c;
    }
}

char *decode(const char *data)
{
    size_t data_len = strlen(data);
    size_t dest_size = (data_len * 2) + 1;
    char *decoded = malloc(dest_size);

    unsigned int count = 0;
    size_t used = 0;
    for (; *data != '\0'; ++data) {
        if (is_digit(*data)) {
            count = count * 10 + digit_value(*data);
        } else {
            count = count != 0 ? count : 1;
            expand_dest(&decoded, &dest_size, data_len, used, count);
            decode_char(decoded + used, *data, count);
            used += count;
            count = 0;
        }
    }
    *(decoded + used) = '\0';

    return decoded;
}
