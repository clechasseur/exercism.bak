class Pt
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def +(pt)
    Pt.new(x + pt.x, y + pt.y)
  end

  def to_a
    [x, y]
  end
end

class Robot
  BEARINGS = {
    :east => Pt.new(1, 0),
    :west => Pt.new(-1, 0),
    :north => Pt.new(0, 1),
    :south => Pt.new(0, -1)
  }
  CLOCKWISE_BEARINGS = %i[north east south west]

  attr_reader :pt, :bearing

  def initialize
    @pt = Pt.new(0, 0)
    @bearing = :north
  end

  def orient(direction)
    raise ArgumentError, "Invalid bearing: #{direction}" unless BEARINGS.has_key? direction
    @bearing = direction
  end

  def turn_right
    @bearing = CLOCKWISE_BEARINGS[(CLOCKWISE_BEARINGS.index(bearing) + 1) % CLOCKWISE_BEARINGS.size]
  end

  def turn_left
    @bearing = CLOCKWISE_BEARINGS[(CLOCKWISE_BEARINGS.index(bearing) + CLOCKWISE_BEARINGS.size - 1) % CLOCKWISE_BEARINGS.size]
  end

  def advance
    @pt += BEARINGS[bearing]
  end

  def at(x, y)
    @pt = Pt.new(x, y)
  end

  def coordinates
    pt.to_a
  end
end

class Simulator
  INSTRUCTIONS = {
    "L" => :turn_left,
    "R" => :turn_right,
    "A" => :advance
  }

  def instructions(input)
    input.chars.map { |c| INSTRUCTIONS[c] }
  end

  def place(robot, x:, y:, direction:)
    robot.at(x, y)
    robot.orient(direction)
  end

  def evaluate(robot, input)
    instructions(input).each do |i|
      robot.method(i).call
    end
  end
end
