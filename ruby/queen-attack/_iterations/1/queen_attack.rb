class Queens
  def initialize(white:, black: nil)
    validate_queen(white)
    validate_queen(black) unless black.nil?

    @white = white
    @black = black
  end

  def attack?
    @white[0] == @black[0] ||
    @white[1] == @black[1] ||
    (@white[0] - @black[0]).abs == (@white[1] - @black[1]).abs
  end

  private

  def validate_queen(queen)
    raise ArgumentError, "Invalid position: #{queen}" unless queen.all? { (0..7) === _1 }
  end
end
