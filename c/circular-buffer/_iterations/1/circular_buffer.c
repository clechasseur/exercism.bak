#include "circular_buffer.h"

#include <errno.h>
#include <stdlib.h>

#define ERR_CHECK(cond, errval) \
    if (cond) {                 \
        errno = errval;         \
        return EXIT_FAILURE;    \
    }

struct circular_buffer {
    buffer_value_t *data;
    size_t capacity;
    size_t size;
    buffer_value_t *rp;
    buffer_value_t *wp;
};

static buffer_value_t *inc_mod(buffer_value_t **p, size_t capacity, buffer_value_t *data)
{
    buffer_value_t *pre = *p;

    if (++*p == data + capacity) {
        *p = data;
    }

    return pre;
}

circular_buffer_t *new_circular_buffer(size_t capacity)
{
    errno = 0;

    circular_buffer_t *buffer = (circular_buffer_t*) malloc(sizeof(circular_buffer_t));
    if (buffer == NULL) {
        return NULL;
    }

    buffer->capacity = capacity;
    buffer->data = (buffer_value_t*) malloc(capacity * sizeof(buffer_value_t));
    if (buffer->data == NULL) {
        free(buffer);
        return NULL;
    }

    clear_buffer(buffer);

    return buffer;
}

buffer_result_t delete_buffer(circular_buffer_t *buffer)
{
    errno = 0;

    ERR_CHECK(buffer == NULL, EINVAL);

    free(buffer->data);
    free(buffer);

    return EXIT_SUCCESS;
}

buffer_result_t read(circular_buffer_t *buffer, buffer_value_t *value)
{
    errno = 0;

    ERR_CHECK(buffer == NULL, EINVAL);
    ERR_CHECK(buffer->size == 0, ENODATA);

    *value = *inc_mod(&buffer->rp, buffer->capacity, buffer->data);
    --buffer->size;

    return EXIT_SUCCESS;
}

buffer_result_t write(circular_buffer_t *buffer, buffer_value_t value)
{
    errno = 0;

    ERR_CHECK(buffer == NULL, EINVAL);
    ERR_CHECK(buffer->size == buffer->capacity, ENOBUFS);

    return overwrite(buffer, value);
}

buffer_result_t overwrite(circular_buffer_t *buffer, buffer_value_t value)
{
    errno = 0;

    ERR_CHECK(buffer == NULL, EINVAL);

    if (buffer->size == buffer->capacity) {
        inc_mod(&buffer->rp, buffer->capacity, buffer->data);
    } else {
        ++buffer->size;
    }
    *inc_mod(&buffer->wp, buffer->capacity, buffer->data) = value;

    return EXIT_SUCCESS;
}

buffer_result_t clear_buffer(circular_buffer_t *buffer)
{
    errno = 0;

    ERR_CHECK(buffer == NULL, EINVAL);

    buffer->size = 0;
    buffer->rp = buffer->data;
    buffer->wp = buffer->data;

    return EXIT_SUCCESS;
}
