class AssemblyLine
  BASE_PRODUCTION_RATE = 221.0
  SUCCESS_RATE_BY_SPEED = {
    1 => 1.0, 2 => 1.0, 3 => 1.0, 4 => 1.0,
    5 => 0.9, 6 => 0.9, 7 => 0.9, 8 => 0.9,
    9 => 0.8,
    10 => 0.77
  }
  MINUTES_PER_HOUR = 60.0

  def initialize(speed)
    @speed = speed
  end

  def production_rate_per_hour
    BASE_PRODUCTION_RATE * @speed * SUCCESS_RATE_BY_SPEED[@speed]
  end

  def working_items_per_minute
    (production_rate_per_hour / MINUTES_PER_HOUR).floor
  end
end
