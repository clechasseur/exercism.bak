#ifndef CLOCK_H
#define CLOCK_H

#include <stdbool.h>

#define MAX_CLOCK_STR_LEN sizeof("##:##")

typedef union {
    char text[MAX_CLOCK_STR_LEN];
    struct {
        char _text[MAX_CLOCK_STR_LEN];
        int minutes_in_day;
    } state;
} clock_t;

clock_t clock_create(int hour, int minute);
clock_t clock_add(clock_t clock, int minute_add);
clock_t clock_subtract(clock_t clock, int minute_subtract);
bool clock_is_equal(clock_t a, clock_t b);

#endif
