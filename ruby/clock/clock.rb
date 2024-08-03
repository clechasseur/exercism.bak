class Clock
  HOUR_PER_DAY = 24
  MINUTE_PER_HOUR = 60

  attr_reader :hour, :minute

  def initialize(hour: 0, minute: 0)
    while minute >= MINUTE_PER_HOUR
      hour += 1
      minute -= MINUTE_PER_HOUR
    end
    while minute < 0
      hour -= 1
      minute += MINUTE_PER_HOUR
    end
    while hour >= HOUR_PER_DAY
      hour -= HOUR_PER_DAY
    end
    while hour < 0
      hour += HOUR_PER_DAY
    end

    @hour = hour
    @minute = minute
  end

  def to_s
    "#{hour.to_s.rjust(2, '0')}:#{minute.to_s.rjust(2, '0')}"
  end

  def ==(other)
    hour == other.hour && minute == other.minute
  end

  def +(other)
    Clock.new(hour: hour + other.hour, minute: minute + other.minute)
  end

  def -(other)
    Clock.new(hour: hour - other.hour, minute: minute - other.minute)
  end
end
