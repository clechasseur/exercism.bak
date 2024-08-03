#include "clock.h"

#include <stdio.h>

const int MINUTES_PER_HOUR = 60;
const int MINUTES_PER_DAY = 24 * MINUTES_PER_HOUR;

// C modulo operator (`%`) is non-Euclidian, so we roll our own.
static inline int rem_euclid(int n, int divisor)
{
    return (n % divisor + divisor) % divisor;
}

static inline int clock_minutes_in_day(const clock_t *clock)
{
    return clock->state.minutes_in_day;
}

clock_t clock_create(int hour, int minute)
{
    clock_t clock = {
        .state = { .minutes_in_day = rem_euclid(hour * MINUTES_PER_HOUR + minute, MINUTES_PER_DAY) }
    };

    // This is annoying, because the compiler warns that the output may be
    // truncated - it's not true in our case, but even if it was, it's safe
    // because we're using `snprintf` and not `sprintf`.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-truncation" 
    snprintf(clock.text, MAX_CLOCK_STR_LEN,
             "%.2d:%.2d",
             clock_minutes_in_day(&clock) / MINUTES_PER_HOUR,
             clock_minutes_in_day(&clock) % MINUTES_PER_HOUR);
#pragma GCC diagnostic pop

    return clock;
}

clock_t clock_add(clock_t clock, int minute_add)
{
    return clock_create(0, clock_minutes_in_day(&clock) + minute_add);
}

clock_t clock_subtract(clock_t clock, int minute_subtract)
{
    return clock_add(clock, -minute_subtract);
}

bool clock_is_equal(clock_t a, clock_t b)
{
    return clock_minutes_in_day(&a) == clock_minutes_in_day(&b);
}
