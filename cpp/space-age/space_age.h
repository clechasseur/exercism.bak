#if !defined(SPACE_AGE_H)
#define SPACE_AGE_H

namespace space_age {

constexpr double earth_year_in_seconds = 31557600.0;

constexpr double mercury_year_in_seconds = earth_year_in_seconds * 0.2408467;
constexpr double venus_year_in_seconds = earth_year_in_seconds * 0.61519726;
constexpr double mars_year_in_seconds = earth_year_in_seconds * 1.8808158;
constexpr double jupiter_year_in_seconds = earth_year_in_seconds * 11.862615;
constexpr double saturn_year_in_seconds = earth_year_in_seconds * 29.447498;
constexpr double uranus_year_in_seconds = earth_year_in_seconds * 84.016846;
constexpr double neptune_year_in_seconds = earth_year_in_seconds * 164.79132;

class space_age
{
public:
    explicit space_age(unsigned long long seconds)
        : seconds_{seconds}
    {}

    auto seconds() const -> unsigned long long {
        return seconds_;
    }

    auto on_mercury() const -> double {
        return on_planet(mercury_year_in_seconds);
    }
    auto on_venus() const -> double {
        return on_planet(venus_year_in_seconds);
    }
    auto on_earth() const -> double {
        return on_planet(earth_year_in_seconds);
    }
    auto on_mars() const -> double {
        return on_planet(mars_year_in_seconds);
    }
    auto on_jupiter() const -> double {
        return on_planet(jupiter_year_in_seconds);
    }
    auto on_saturn() const -> double {
        return on_planet(saturn_year_in_seconds);
    }
    auto on_uranus() const -> double {
        return on_planet(uranus_year_in_seconds);
    }
    auto on_neptune() const -> double {
        return on_planet(neptune_year_in_seconds);
    }
    
private:
    unsigned long long seconds_;

    auto on_planet(double planet_year_in_seconds) const -> double {
        return seconds_ / planet_year_in_seconds;
    }
};

}  // namespace space_age

#endif // SPACE_AGE_H