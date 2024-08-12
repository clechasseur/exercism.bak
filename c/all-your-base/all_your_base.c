#include "all_your_base.h"

static void reverse_digits(int8_t digits[], size_t num_digits)
{
    for (size_t i = 0; i < num_digits / 2; ++i) {
        // Switch two integer values without a temporary variable with this one weird trick!
        digits[i] ^= digits[num_digits - i - 1];
        digits[num_digits - i - 1] ^= digits[i];
        digits[i] ^= digits[num_digits - i - 1];
    }
}

size_t rebase(int8_t digits[], int16_t in_base, int16_t out_base, size_t in_num_digits)
{
    if (in_base < 2 || out_base < 2) {
        return 0;
    }
    
    int64_t n = 0;
    for (int8_t *digit = digits; digit != digits + in_num_digits; ++digit) {
        if (*digit < 0 || *digit >= in_base) {
            return 0;
        }
        n = n * in_base + *digit;
    }

    if (n == 0 && in_num_digits != 0) {
        *digits = 0;
        return 1;
    }

    size_t out_num_digits = 0;
    int8_t *digit = digits;
    while (n > 0 && out_num_digits < DIGITS_ARRAY_SIZE) {
        *digit++ = n % out_base;
        n /= out_base;
        ++out_num_digits;
    }
    reverse_digits(digits, out_num_digits);

    return out_num_digits;
}
