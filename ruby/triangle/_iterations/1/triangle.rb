class Triangle
  attr_reader :sides

  def initialize(sides)
    @sides = sides
  end

  def valid
    sides.all? { |side| side > 0 && sides.sum >= side * 2 }
  end

  def equilateral?
    valid && sides.uniq.size == 1
  end

  def isosceles?
    valid && sides.uniq.size <= 2
  end

  def scalene?
    valid && sides.uniq.size == 3
  end
end
