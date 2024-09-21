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
    ((min_factor + 2)..max_factor).flat_map do |f2|
      ((min_factor + 1)...f2).flat_map do |f1|
        (min_factor...f1).map { |f0| new(f0, f1, f2) }
      end
    end.filter do |t|
      t.pythagorean? && (sum.nil? || t.sum == sum)
    end
  end
end
