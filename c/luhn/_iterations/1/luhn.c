#include "luhn.h"

#include <ctype.h>
#include <string.h>

bool luhn(const char *num)
{
    int sum = 0;
    size_t digit_count = 0;
    for (const char *rnum = num + (strlen(num) - 1); rnum >= num; --rnum) {
        if (*rnum == ' ') {
            continue;
        }
        if (!isdigit((unsigned char) *rnum)) {
            return false;
        }

        int digit = *rnum - '0';
        if (++digit_count % 2 == 0) {
            digit *= 2;
            if (digit > 9) {
                digit -= 9;
            }
        }
        sum += digit;
    }

    return digit_count > 1 && sum % 10 == 0;
}
