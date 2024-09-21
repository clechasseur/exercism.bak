#include "clock.h"

#include <iomanip>
#include <sstream>

namespace date_independent {

namespace {

constexpr int minutes_per_hour = 60;
constexpr int minutes_per_day = 24 * minutes_per_hour;

// C++ modulo operator (%) is non-Euclidian, so we provide our own.
auto rem_euclid(int n, int divisor) -> int {
    return (n % divisor + divisor) % divisor;
}

} // anonymous namespace

auto clock::at(int hours, int minutes) -> clock {
    return {hours, minutes};
}

auto clock::hours() const -> int {
    return minutes_in_day / minutes_per_hour;
}

auto clock::minutes() const -> int {
    return minutes_in_day % minutes_per_hour;
}

clock::operator std::string() const {
    std::ostringstream oss;
    oss << std::setfill('0')
        << std::setw(2) << hours()
        << ':'
        << std::setw(2) << minutes();
    return oss.str();
}

auto clock::plus(int minutes_to_add) const -> clock {
    return {0, minutes_in_day + minutes_to_add};
}

auto operator==(const clock& c1, const clock& c2) -> bool {
    return c1.minutes_in_day == c2.minutes_in_day;
}

auto operator!=(const clock& c1, const clock& c2) -> bool {
    return !(c1 == c2);
}

clock::clock(int hours, int minutes)
    : minutes_in_day{rem_euclid(hours * minutes_per_hour + minutes, minutes_per_day)}
{
}

}  // namespace date_independent
