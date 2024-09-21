class Darts
  INNER_CIRCLE = 0.0..1.0
  MIDDLE_CIRCLE = 0.0..5.0
  OUTER_CIRCLE = 0.0..10.0

  INNER_CIRCLE_SCORE = 10
  MIDDLE_CIRCLE_SCORE = 5
  OUTER_CIRCLE_SCORE = 1

  attr_reader :score

  def initialize(x, y)
    case distance(x.to_f, y.to_f)
    when INNER_CIRCLE
      @score = INNER_CIRCLE_SCORE
    when MIDDLE_CIRCLE
      @score = MIDDLE_CIRCLE_SCORE
    when OUTER_CIRCLE
      @score = OUTER_CIRCLE_SCORE
    else
      @score = 0
    end
  end

  private

  def distance(x, y)
    Math.sqrt(x ** 2 + y ** 2)
  end
end
