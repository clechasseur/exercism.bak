from decimal import Decimal

class SpaceAge:
    def __init__(self, seconds):
        self.earth_years = seconds / EARTH_YEAR_IN_SECONDS

EARTH_YEAR_IN_SECONDS = 31557600.0
ORBITAL_PERIODS = {
    'mercury': 0.2408467,
    'venus': 0.61519726,
    'earth': 1.0,
    'mars': 1.8808158,
    'jupiter': 11.862615,
    'saturn': 29.447498,
    'uranus': 84.016846,
    'neptune': 164.79132,
}

def round_to_two_decimals(n):
    return float(Decimal(n).quantize(Decimal('.01')))

def make_on_planet(orbital_period):
    def on_planet(self):
        return round_to_two_decimals(self.earth_years / orbital_period)

    return on_planet

for planet, orbital_period in ORBITAL_PERIODS.items():
    setattr(SpaceAge, "on_{}".format(planet), make_on_planet(orbital_period))
