#include "space_age.h"

#include <math.h>

#define EARTH_YEAR_IN_SECONDS   31557600.0

#define ORBITAL_PERIOD_MERCURY  0.2408467
#define ORBITAL_PERIOD_VENUS    0.61519726
#define ORBITAL_PERIOD_EARTH    1.0
#define ORBITAL_PERIOD_MARS     1.8808158
#define ORBITAL_PERIOD_JUPITER  11.862615
#define ORBITAL_PERIOD_SATURN   29.447498
#define ORBITAL_PERIOD_URANUS   84.016846
#define ORBITAL_PERIOD_NEPTUNE  164.79132

#define INVALID_PLANET          -1.0

#define CASE_PLANET(planet) \
    case planet: \
        return ORBITAL_PERIOD_ ## planet

static float orbital_period(planet_t planet)
{
    switch (planet) {
        CASE_PLANET(MERCURY);
        CASE_PLANET(VENUS);
        CASE_PLANET(EARTH);
        CASE_PLANET(MARS);
        CASE_PLANET(JUPITER);
        CASE_PLANET(SATURN);
        CASE_PLANET(URANUS);
        CASE_PLANET(NEPTUNE);
    }

    return INVALID_PLANET;
}

float age(planet_t planet, int64_t seconds)
{
    float period = orbital_period(planet);
    return period != INVALID_PLANET
        ? seconds / EARTH_YEAR_IN_SECONDS / period
        : INVALID_PLANET;
}
