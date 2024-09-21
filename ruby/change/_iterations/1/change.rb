# Port of my Kotlin solution:
# https://exercism.org/tracks/kotlin/exercises/change/solutions/clechasseur

module Change
  class NoCoinsError < StandardError; end
  class UnsortedCoinsError < StandardError; end
  class NonPositiveCoinError < StandardError; end
  class DuplicateCoinsError < StandardError; end
  class NegativeTargetError < StandardError; end
  class ImpossibleCombinationError < StandardError; end

  def self.generate(coins, amount)
    raise NoCoinsError, "Must have at least one coin type" if coins.empty?
    raise UnsortedCoinsError, "Coin types must be sorted" unless coins.sort == coins
    raise NonPositiveCoinError, "Coin types must be positive" unless coins.all?(&:positive?)
    raise DuplicateCoinsError, "Coin types must be unique" unless coins.uniq == coins
    raise NegativeTargetError, "Target amount must not be negative" if amount.negative?

    change = nil
    if coins.first == 1
      change = possibly_generate(coins.drop(1).reverse, amount)
    end
    if change.nil?
      change = possibly_generate(coins.reverse, amount)
    end
    raise ImpossibleCombinationError, "The total #{amount} cannot be represented in the given currency" if change.nil?
    change.sort
  end

  private

  def self.possibly_generate(coins, amount, steps_left = 2 ** 63)
    return [] if amount.zero?
    return nil if steps_left.zero?

    change = nil
    coins.each do |coin|
      if amount >= coin
        sub_change = possibly_generate(
          coins.drop_while { _1 > coin },
          amount - coin,
          [steps_left - 1, (change.nil? ? 2 ** 63 : change.size) - 1].min
        )
        if !sub_change.nil? && (change.nil? || (sub_change.size + 1 < change.size))
          change = [coin] + sub_change
        end
      end
    end
    change
  end
end
