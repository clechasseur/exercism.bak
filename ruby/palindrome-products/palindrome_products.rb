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
        palindrome = Palindrome.new(product, [[a, b]])
        if @smallest.nil? || palindrome.value < @smallest.value
          @smallest = palindrome
        elsif @smallest.value == palindrome.value
          @smallest.factors << palindrome.factors.first
        end

        if @largest.nil? || palindrome.value > @largest.value
          @largest = palindrome
        elsif @largest.value == product
          @largest.factors << palindrome.factors.first
        end
      end
    end
  end

  private

  def palindrome?(n)
    n.to_s.reverse == n.to_s
  end
end
