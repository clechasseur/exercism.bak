#include "sieve.h"

#include <stdbool.h>
#include <stdlib.h>

// UINT64_WIDTH is defined in <stdint.h> in C23
#ifndef UINT64_WIDTH
#   define UINT64_WIDTH (8 * sizeof(uint64_t))
#endif

static uint64_t *alloc_bits(size_t size)
{
    size_t num = size / UINT64_WIDTH;
    if (num * UINT64_WIDTH < size) {
        ++num;
    }

    return calloc(num, sizeof(uint64_t));
}

static void free_bits(uint64_t *bits)
{
    free(bits);
}

static bool get_bit(uint64_t *bits, size_t idx)
{
    bits += idx / UINT64_WIDTH;
    idx %= UINT64_WIDTH;

    return (*bits & (UINT64_C(1) << idx)) != 0; 
}

static void set_bit(uint64_t *bits, size_t idx)
{
    bits += idx / UINT64_WIDTH;
    idx %= UINT64_WIDTH;

    *bits |= (UINT64_C(1) << idx);
}

uint32_t sieve(uint32_t limit, uint32_t *primes, size_t max_primes)
{
    uint64_t *bits = alloc_bits(limit - 1);
    uint32_t primes_count = 0;

    for (uint32_t candidate = 2; candidate <= limit && primes_count < max_primes; ++candidate) {
        if (!get_bit(bits, candidate - 2)) {
            *primes++ = candidate;
            if (++primes_count < max_primes) {
                for (uint32_t i = candidate * 2; i <= limit; i += candidate) {
                    set_bit(bits, i - 2);
                }
            }
        }
    }

    free_bits(bits);
    return primes_count;
}
