const EARTH_YEAR_IN_SECONDS = 31557600;
const ORBITAL_PERIODS = {
    'earth': 1.0,
    'mercury': 0.2408467,
    'venus': 0.61519726,
    'mars': 1.8808158,
    'jupiter': 11.862615,
    'saturn': 29.447498,
    'uranus': 84.016846,
    'neptune': 164.79132
};

export function age(planet, onEarthInSeconds) {
    return Number((onEarthInSeconds / EARTH_YEAR_IN_SECONDS / ORBITAL_PERIODS[planet]).toFixed(2));
}
