#include "utils.h"

uint8_t popcount(uint8_t value)
{
    uint8_t pop = 0;
    
    while (value != 0) {
        if ((value & 1) != 0) {
            ++pop;
        }
        value >>= 1;
    }

    return pop;
}

uint8_t pop_bit(uint8_t *value)
{
    if (value == 0) {
        return 0;
    }

    uint8_t bit = 1;
    while ((*value & bit) == 0) {
        bit <<= 1;
    }

    *value &= ~bit;
    return bit;
}
