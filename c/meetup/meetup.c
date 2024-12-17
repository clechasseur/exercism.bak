#include "meetup.h"

#include <stdbool.h>
#include <string.h>
#include <time.h>

// Valid values for the `week` parameter for `meetup_day_of_month`.
static const char *WEEKS[] = {
    "first",
    "second",
    "third",
    "fourth",
    "last",
    "teenth"
};

// Enum corresponding to the `week` parameter values for `meetup_day_of_month`.
enum week_t {
    WEEK_INVALID = -1,
    WEEK_FIRST = 0,
    WEEK_SECOND,
    WEEK_THIRD,
    WEEK_FOURTH,
    WEEK_LAST,
    WEEK_TEENTH,
    WEEK_END
};

// Valid values for the `day_of_week` parameter for `meetup_day_of_month`.
// They are ordered as to correspond to the values for the `tm_wday` field of a `struct tm`.
static const char *DAYS_OF_WEEK[] = {
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
};

// Number of days in every month.
static const int DAYS_IN_MONTH[] = {
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

// Converts a value passed to the `week` parameter of `meetup_day_of_month` into an `enum week_t`.
static enum week_t get_week_t(const char *week)
{
    for (enum week_t w = WEEK_FIRST; w < WEEK_END; ++w) {
        if (strcmp(week, WEEKS[w]) == 0) {
            return w;
        }
    }

    return WEEK_INVALID;
}

// Converts a value passed to the `day_of_week` parameter of `meetup_day_of_month`
// into an int value that can be used in the `tm_wday` field of a `struct tm`.
static int get_tm_wday(const char *day_of_week)
{
    for (int wday = 0; wday < 7; ++wday) {
        if (strcmp(day_of_week, DAYS_OF_WEEK[wday]) == 0) {
            return wday;
        }
    }

    return -1;
}

// Determines if the given year is a leap year.
static bool is_leap_year(unsigned int year)
{
    return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

// Returns the last day of the given month.
static int get_last_day_of_month(unsigned int year, unsigned int month)
{
    int day = DAYS_IN_MONTH[month - 1];
    if (month == 2 && is_leap_year(year)) {
        ++day;
    }
    return day;
}

// Given year, month and day of week, returns the corresponding "teenth" day of the month.
static int teenth_meetup_day_of_month(unsigned int year,
                                      unsigned int month,
                                      int wday)
{
    // Get information about the first "teenth" day of the month.
    struct tm first_teenth = {
        .tm_mday = 13,
        .tm_mon = month - 1,
        .tm_year = year - 1900
    };
    mktime(&first_teenth);

    // Advance the required number of days and return day of month.
    int wday_diff = first_teenth.tm_wday <= wday
        ? wday - first_teenth.tm_wday
        : 7 - (first_teenth.tm_wday - wday);
    return first_teenth.tm_mday + wday_diff;
}

// Given year, month and day of week, returns the corresponding day of month in the last week of the month.
static int last_week_meetup_day_of_month(unsigned int year,
                                         unsigned int month,
                                         int wday)
{
    // Get information about the last day of the month.
    struct tm last_day = {
        .tm_mday = get_last_day_of_month(year, month),
        .tm_mon = month - 1,
        .tm_year = year - 1900
    };
    mktime(&last_day);

    // Go back the required number of days and return day of month.
    int wday_diff = last_day.tm_wday >= wday
        ? last_day.tm_wday - wday
        : 7 - (wday - last_day.tm_wday);
    return last_day.tm_mday - wday_diff;
}

// Given year, month, week and day of week identifiers, returns the corresponding day of the month. 
int meetup_day_of_month(unsigned int year,
                        unsigned int month,
                        const char *week,
                        const char *day_of_week)
{
    // Convert string values into corresponding enums.
    enum week_t week_e = get_week_t(week);
    int wday = get_tm_wday(day_of_week);

    // Handle special cases.
    if (week_e == WEEK_TEENTH) {
        return teenth_meetup_day_of_month(year, month, wday);
    }
    if (week_e == WEEK_LAST) {
        return last_week_meetup_day_of_month(year, month, wday);
    }

    // Get information about the first day of the month.
    struct tm first_day = {
        .tm_mday = 1,
        .tm_mon = month - 1,
        .tm_year = year - 1900
    };
    mktime(&first_day);

    // Advance the required number of days and weeks, then return day of month.
    int wday_diff = first_day.tm_wday <= wday
        ? wday - first_day.tm_wday
        : 7 - (first_day.tm_wday - wday);
    int week_diff = 7 * week_e;
    return 1 + wday_diff + week_diff;
}
