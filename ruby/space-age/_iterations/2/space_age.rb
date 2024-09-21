class SpaceAge
  EARTH_YEAR_IN_SECONDS = 31557600
  ORBITAL_PERIODS = {
    "mercury" => 0.2408467,
    "venus" => 0.61519726,
    "earth" => 1.0,
    "mars" => 1.8808158,
    "jupiter" => 11.862615,
    "saturn" => 29.447498,
    "uranus" => 84.016846,
    "neptune" => 164.79132
  }

  def initialize(earth_age)
    @earth_age = earth_age.to_f / EARTH_YEAR_IN_SECONDS
  end

  ORBITAL_PERIODS.each do |planet, period|
    define_method("on_#{planet}") { @earth_age / period }
  end
end
