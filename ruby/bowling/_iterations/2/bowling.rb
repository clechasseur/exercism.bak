class Game
  FRAMES_PER_GAME = 10
  ROLLS_PER_FRAME = 2
  FILL_BALL = 1
  NUM_PINS = 10
  SPARE_SCORE = 10
  STRIKE_SCORE = 10
  NEXT_ROLL = 0...1
  NEXT_TWO_ROLLS = 0...2

  class BowlingError < StandardError; end

  def initialize
    @frames = [Frame.new]
  end

  def done?
    @frames.size == FRAMES_PER_GAME && @frames.last.done?
  end

  def roll(pins)
    raise BowlingError, "Game is done" if done?
    raise BowlingError, "Roll must knock down 0 or more pins" if pins.negative?

    @frames.last.roll(pins)
    if @frames.last.done? && @frames.size < FRAMES_PER_GAME
      @frames << Frame.new(last: @frames.size == FRAMES_PER_GAME - 1)
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
      @max_rolls = ROLLS_PER_FRAME
      @last = last
    end

    def done?
      rolls.size == @max_rolls || (!last && rolls.first == NUM_PINS)
    end

    def roll(pins)
      raise BowlingError, "Single roll cannot knock down more than #{NUM_PINS} pins" if pins > NUM_PINS
      raise BowlingError, "Frame cannot knock down more than #{NUM_PINS} pins" if !last && rolls.sum + pins > NUM_PINS
      raise BowlingError, "Last frame cannot knock down more than #{NUM_PINS} pins if spare" if last && rolls.size == 1 && rolls.first < NUM_PINS && rolls.first + pins > NUM_PINS
      raise BowlingError, "Last frame cannot knock down more than #{NUM_PINS} pins if fill ball is spare" if last && rolls.size == 2 && rolls.first == NUM_PINS && rolls[1] < NUM_PINS && rolls[1] + pins > NUM_PINS

      rolls << pins
      if last && @max_rolls == ROLLS_PER_FRAME && rolls.sum == NUM_PINS
        @max_rolls += FILL_BALL
      end
    end

    def score(next_rolls)
      if !last && rolls.first == NUM_PINS
        STRIKE_SCORE + next_rolls[NEXT_TWO_ROLLS].sum
      elsif !last && rolls.sum == NUM_PINS
        SPARE_SCORE + next_rolls[NEXT_ROLL].sum
      else
        rolls.sum
      end
    end
  end
end
