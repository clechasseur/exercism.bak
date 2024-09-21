#include "scrabble_score.h"

#include <ctype.h>

// Letter                           Value
// A, E, I, O, U, L, N, R, S, T       1
// D, G                               2
// B, C, M, P                         3
// F, H, V, W, Y                      4
// K                                  5
// J, X                               8
// Q, Z                               10

unsigned int SCORES[] = { 1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10 };

unsigned int score(const char *word)
{
    unsigned int total = 0;

    for (; *word != '\0'; ++word) {
        // Note: here, the cast to `unsigned char` is important because
        // if the character cannot be represented as an unsigned char,
        // we get undefined behavior; see https://en.cppreference.com/w/c/string/byte/toupper
        total += SCORES[toupper((unsigned char) *word) - 'A'];
    }

    return total;
}
