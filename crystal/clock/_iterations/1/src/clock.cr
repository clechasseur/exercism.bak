class Clock
  include Comparable(Int32)

  MINUTES_PER_DAY = 60 * 24

  @minutes : Int32

  def initialize(hour : Int32 = 0, minute : Int32 = 0)
    @minutes = (hour * 60 + minute) % MINUTES_PER_DAY
  end

  def hour
    @minutes // 60
  end

  def minute
    @minutes % 60
  end

  def to_s
    "%02d:%02d" % [hour, minute]
  end

  def ==(other)
    @minutes == other.@minutes
  end

  def <=>(other)
    @minutes <=> other.@minutes
  end

  def +(other : Int32)
    Clock.new(minute: @minutes + other)
  end

  def +(other : Clock)
    self + other.@minutes
  end

  def -(other : Int32)
    self + -other
  end

  def -(other : Clock)
    self - other.@minutes
  end
end
