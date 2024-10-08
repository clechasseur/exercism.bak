seconds_in_earth_year <- 31557600
seconds_in_planet_years = c(earth   = 1.0,
                            mercury = 0.2408467,
                            venus   = 0.61519726,
                            mars    = 1.8808158,
                            jupiter = 11.862615,
                            saturn  = 29.447498,
                            uranus  = 84.016846,
                            neptune = 164.79132) * seconds_in_earth_year

space_age <- function(seconds, planet) {
  unname(round(seconds / seconds_in_planet_years[planet], 2))
}
