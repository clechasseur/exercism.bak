class Palindromes
  Palindrome = Struct.new(:value, :factors)

  attr_reader :smallest, :largest

  def initialize(min_factor: 1, max_factor:)
    @min_factor = min_factor
    @max_factor = max_factor
  end

  def generate
    (@min_factor..@max_factor).each do |a|
      (a..@max_factor).each do |b|
        product = a * b
        if is_palindrome(product)
          palindrome = Palindrome.new(product, [[a, b]])
          if @smallest.nil? || palindrome.value < @smallest.value
            @smallest = palindrome
          elsif @smallest.value == palindrome.value
            @smallest.factors << palindrome.factors.first
          end

          if @largest.nil? || palindrome.value > @largest.value
            @largest = palindrome
          elsif @largest.value == palindrome.value
            @largest.factors << palindrome.factors.first
          end
        end
      end
    end
  end

  private

  def is_palindrome(n)
    s = n.to_s
    s == s.reverse
  end
end
