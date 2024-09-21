#ifndef CIRCULAR_BUFFER_H
#define CIRCULAR_BUFFER_H

#include <stddef.h>
#include <stdint.h>

#define EXIT_SUCCESS 1
#define EXIT_FAILURE 0

typedef int32_t buffer_value_t;
typedef int16_t buffer_result_t;

typedef struct circular_buffer circular_buffer_t;

circular_buffer_t *new_circular_buffer(size_t capacity);
buffer_result_t delete_buffer(circular_buffer_t *buffer);

buffer_result_t read(circular_buffer_t *buffer, buffer_value_t *value);
buffer_result_t write(circular_buffer_t *buffer, buffer_value_t value);
buffer_result_t overwrite(circular_buffer_t *buffer, buffer_value_t value);
buffer_result_t clear_buffer(circular_buffer_t *buffer);

#endif
