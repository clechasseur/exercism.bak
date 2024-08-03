#include "hamming.h"

#include <stddef.h>

int compute(const char *lhs, const char *rhs)
{
    int score = 0;

    if (lhs == NULL || rhs == NULL) {
        return -1;
    }

    for (; *lhs != '\0' && *rhs != '\0'; ++lhs, ++rhs) {
        if (*lhs != *rhs) {
            ++score;
        }
    }

    return (*lhs == '\0' && *rhs == '\0') ? score : -1;
}
