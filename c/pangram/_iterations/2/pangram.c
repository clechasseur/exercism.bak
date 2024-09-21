#include "pangram.h"

#include <ctype.h>
#include <stddef.h>

// This produces a bitfield with all 26 least-significant bits set
const int ALL_LETTERS = (1 << 26) - 1;

bool is_pangram(const char *sentence)
{
    int letters = 0;

    if (sentence == NULL) {
        return false;
    }
    
    for (; *sentence != '\0'; ++sentence) {
        if (isalpha((unsigned char) *sentence)) {
            letters |= 1 << (tolower((unsigned char) *sentence) - 'a');
            if (letters == ALL_LETTERS) {
                return true;
            }
        }
    }

    return false;
}
