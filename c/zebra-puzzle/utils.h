#ifndef UTILS_H
#define UTILS_H

#include <stdint.h>

// Counts the number of set bits in the given value.
uint8_t popcount(uint8_t value);

// Pops the next least-significant bit from the given value and returns it.
// If the value has no bit set, returns 0.
//
// Note: the return value is the bit, not its index. For example:
//
// ```c
// uint8_t value = 0x6;           // Binary 0110
// uint8_t bit = pop_bit(&value); // bit = 0x2 (0010), value = 0x4 (0100)
// ```
uint8_t pop_bit(uint8_t *value);

#endif // UTILS_H
