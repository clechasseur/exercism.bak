#include "roman_numerals.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ROMAN_DIGITS   4 // Max number of roman digits for one decimal digit
#define MAX_DECIMAL_DIGITS 4 // Max number of decimal digits in number

typedef struct {
    char one, five, ten;
} roman_digit_infos_t;

static const roman_digit_infos_t ROMAN_DIGIT_INFOS[] = {
    { 'I', 'V', 'X' },
    { 'X', 'L', 'C' },
    { 'C', 'D', 'M' },
    { 'M', '!', '!' }
};

typedef struct {
    char digits[MAX_ROMAN_DIGITS];
    size_t count;
} roman_digits_t;

static void fill_roman_digits(
    roman_digits_t *roman_digits,
    const roman_digit_infos_t *roman_digit_infos,
    unsigned int digit
) {
    if (digit == 9) {
        roman_digits->digits[0] = roman_digit_infos->one;
        roman_digits->digits[1] = roman_digit_infos->ten;
        roman_digits->count = 2;
    } else if (digit >= 5) {
        roman_digits->digits[0] = roman_digit_infos->five;
        memset(roman_digits->digits + 1, roman_digit_infos->one, digit - 5);
        roman_digits->count = digit - 4;
    } else if (digit == 4) {
        roman_digits->digits[0] = roman_digit_infos->one;
        roman_digits->digits[1] = roman_digit_infos->five;
        roman_digits->count = 2;
    } else {
        memset(roman_digits->digits, roman_digit_infos->one, digit);
        roman_digits->count = digit;
    }
}

static size_t total_roman_digits_count(
    const roman_digits_t *roman_digits,
    size_t roman_digits_count
) {
    size_t count = 0;

    while (roman_digits_count-- != 0) {
        count += (roman_digits++)->count;
    }

    return count;
}

static char *roman_digits_to_string(
    const roman_digits_t *roman_digits,
    size_t roman_digits_count
) {
    size_t total_digits_count = total_roman_digits_count(roman_digits, roman_digits_count);
    char *as_string = (char*) malloc(total_digits_count + 1);
    char *buff = as_string;

    while (roman_digits_count-- != 0) {
        const roman_digits_t *cur_roman_digits = roman_digits + roman_digits_count;
        strncpy(buff, cur_roman_digits->digits, cur_roman_digits->count);
        buff += cur_roman_digits->count;
    }

    *buff = '\0';
    return as_string;
}

char *to_roman_numeral(unsigned int number)
{
    roman_digits_t roman_digits[MAX_DECIMAL_DIGITS];
    size_t roman_digits_count = 0;
    const roman_digit_infos_t *roman_digit_infos = ROMAN_DIGIT_INFOS;
    
    while (number > 0) {
        fill_roman_digits(
            roman_digits + (roman_digits_count++),
            roman_digit_infos++,
            number % 10
        );
        number /= 10;
    }

    return roman_digits_to_string(roman_digits, roman_digits_count);
}
