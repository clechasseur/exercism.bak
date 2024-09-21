class Palindromes
  Palindrome = Struct.new(:value, :factors)

  attr_reader :smallest, :largest

  def initialize(min_factor: 1, max_factor:)
    @min_factor = min_factor
    @max_factor = max_factor
  end

  def generate
    (@min_factor..@max_factor).to_a.repeated_combination(2).each do |a, b|
      product = a * b
      if palindrome? product
        if @smallest.nil? || product < @smallest.value
          @smallest = Palindrome.new(product, [[a, b]])
        elsif @smallest.value == product
          @smallest.factors << [a, b]
        end

        if @largest.nil? || product > @largest.value
          @largest = Palindrome.new(product, [[a, b]])
        elsif @largest.value == product
          @largest.factors << [a, b]
        end
      end
    end
  end

  private

  def palindrome?(n)
    n.to_s.reverse == n.to_s
  end
end
