#include "binary_search.h"

#include <stddef.h>

const int *binary_search(int value, const int *arr, size_t length)
{
    if (length == 0 || arr == NULL) {
        return NULL;
    }

    const size_t half_length = length / 2;
    const int *middle = arr + half_length;
    return *middle == value
        ? middle
        : binary_search(value, *middle > value ? arr : middle + 1, half_length);
}
