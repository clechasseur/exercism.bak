class Triplet
  attr_reader :factors

  def initialize(*factors)
    @factors = factors
  end

  def sum
    factors.sum
  end

  def product
    factors.reduce(1, &:*)
  end

  def pythagorean?
    factors[0, factors.size - 1].map { |n| n ** 2 }.sum == factors.last ** 2 
  end

  def self.where(min_factor: 1, max_factor:, sum: nil)
    (min_factor..(max_factor - 2)).flat_map do |f0|
      ((f0 + 1)..(max_factor - 1)).flat_map do |f1|
        if sum.nil?
          ((f1 + 1)..max_factor).map { |f2| new(f0, f1, f2) }
        else
          f2 = sum - f0 - f1
          f2 <= max_factor ? [new(f0, f1, f2)] : []
        end
      end
    end.filter(&:pythagorean?)
  end
end
