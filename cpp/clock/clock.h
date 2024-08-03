#if !defined(CLOCK_H)
#define CLOCK_H

#include <string>

namespace date_independent {

class clock
{
public:
    static auto at(int hours, int minutes) -> clock;

    auto hours() const -> int;
    auto minutes() const -> int;

    operator std::string() const;

    auto plus(int minutes_to_add) const -> clock;

    friend auto operator==(const clock& c1, const clock& c2) -> bool;
    friend auto operator!=(const clock& c1, const clock& c2) -> bool;

private:
    int minutes_in_day;

    clock(int hours, int minutes);
};

}  // namespace date_independent

#endif // CLOCK_H