#include "circular_buffer.h"

#include <errno.h>
#include <stdlib.h>

#define ERR_CHECK_RET(cond, errval, ret) \
    if (cond) {                          \
        errno = errval;                  \
        return ret;                      \
    }
#define ERR_CHECK(cond, errval) \
    ERR_CHECK_RET(cond, errval, EXIT_FAILURE)

struct circular_buffer {
    size_t capacity;
    size_t size;
    buffer_value_t *rp;
    buffer_value_t *wp;
    buffer_value_t data[];
};

static buffer_value_t *post_inc_mod(buffer_value_t **p, size_t capacity, buffer_value_t *data)
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

    circular_buffer_t *buffer = (circular_buffer_t*) malloc(sizeof(circular_buffer_t) + sizeof(buffer_value_t) * capacity);
    ERR_CHECK_RET(buffer == NULL, ENOMEM, NULL);

    buffer->capacity = capacity;
    clear_buffer(buffer);

    return buffer;
}

void delete_buffer(circular_buffer_t *buffer)
{
    errno = 0;

    ERR_CHECK_RET(buffer == NULL, EINVAL, );

    free(buffer);
}

buffer_result_t read(circular_buffer_t *buffer, buffer_value_t *value)
{
    errno = 0;

    ERR_CHECK(buffer == NULL, EINVAL);
    ERR_CHECK(buffer->size == 0, ENODATA);

    *value = *post_inc_mod(&buffer->rp, buffer->capacity, buffer->data);
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
        post_inc_mod(&buffer->rp, buffer->capacity, buffer->data);
    } else {
        ++buffer->size;
    }
    *post_inc_mod(&buffer->wp, buffer->capacity, buffer->data) = value;

    return EXIT_SUCCESS;
}

void clear_buffer(circular_buffer_t *buffer)
{
    errno = 0;

    ERR_CHECK_RET(buffer == NULL, EINVAL, );

    buffer->size = 0;
    buffer->rp = buffer->data;
    buffer->wp = buffer->data;
}
