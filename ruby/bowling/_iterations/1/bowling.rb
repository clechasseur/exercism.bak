class Game
  class BowlingError < StandardError; end

  def initialize
    @frames = [Frame.new]
  end

  def done?
    @frames.size == 10 && @frames.last.done?
  end

  def roll(pins)
    raise BowlingError, "Game is done" if done?
    raise BowlingError, "Roll must knock down 0 or more pins" if pins.negative?

    @frames.last.roll(pins)
    if @frames.last.done? && @frames.size < 10
      @frames << Frame.new(last: @frames.size == 9)
    end
  end

  def score
    raise BowlingError, "Cannot calculate score until game is done" unless done?

    @frames.map.with_index do |frame, index|
      frame.score(@frames.drop(index + 1).map(&:rolls).reduce(&:+))
    end.sum
  end

  private

  class Frame
    attr_reader :rolls, :last

    def initialize(last: false)
      @rolls = []
      @max_rolls = 2
      @last = last
    end

    def done?
      rolls.size == @max_rolls || (!last && rolls.first == 10)
    end

    def roll(pins)
      raise BowlingError, "Single roll cannot knock down more than 10 pins" if pins > 10
      raise BowlingError, "Frame cannot knock down more than 10 pins" if !last && (rolls.sum + pins) > 10
      raise BowlingError, "Last frame cannot knock down more than 10 pins if spare" if last && rolls.size == 1 && rolls.first < 10 && rolls.first + pins > 10
      raise BowlingError, "Last frame cannot knock down more than 10 pins if bonus is spare" if last && rolls.size == 2 && rolls.first == 10 && rolls[1] < 10 && rolls[1] + pins > 10

      rolls << pins
      if last && @max_rolls == 2 && rolls.sum == 10
        @max_rolls = 3
      end
    end

    def score(next_rolls)
      if !last && rolls.first == 10
        10 + next_rolls[0, 2].sum
      elsif !last && rolls.sum == 10
        10 + next_rolls[0, 1].sum
      else
        rolls.sum
      end
    end
  end
end
