#include "grains.h"

uint64_t square(uint8_t index)
{
    // Bonus questions left for readers:
    // 1. The tests call this function with index == 0 and index == 65.
    //    The function is expected to return 0 in those cases.
    //    This is not handled by the code below, yet the tests pass. Why?
    // 2. If this function is called with index == 0 initially,
    //    how many times will it call itself recursively?
    
    if (index == 1) {
        return 1;
    } else {
        return square(index - 1) * 2;
    }
}

uint64_t total(void)
{
    uint64_t total = 0;

    for (uint8_t i = 1; i <= 64; ++i) {
        total += square(i);
    }

    return total;
}
