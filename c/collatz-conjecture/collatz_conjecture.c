#include "collatz_conjecture.h"

int steps(int n)
{
    if (n < 1) {
        return ERROR_VALUE;
    } else if (n == 1) {
        return 0;
    }

    return 1 + steps(n % 2 == 0 ? n / 2 : 3 * n + 1);
}
