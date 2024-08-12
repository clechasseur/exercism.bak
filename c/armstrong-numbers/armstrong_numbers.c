#include "armstrong_numbers.h"

#include <math.h>

#define BASE 10

bool is_armstrong_number(int candidate)
{
    if (candidate < 0) {
        return false;
    }
    if (candidate < BASE) {
        return true;
    }
    
    int num_digits = floor(log10(candidate) + 1); // hax
    int sum = 0;
    for (int n = candidate; n != 0; n /= BASE) {
        sum += pow(n % BASE, num_digits);
    }

    return sum == candidate;
}
