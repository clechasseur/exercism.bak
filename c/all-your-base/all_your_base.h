#ifndef ALL_YOUR_BASE_H
#define ALL_YOUR_BASE_H

#include <stddef.h>
#include <stdint.h>

#define DIGITS_ARRAY_SIZE 64

// Given some `digits` expressing a number in base `in_base`,
// converts the number in base `out_base` and stores it in `digits`.
// The `digits` array must be able to store at least `DIGITS_ARRAY_SIZE` elements.
size_t rebase(int8_t digits[], int16_t in_base, int16_t out_base, size_t in_num_digits);

#endif
