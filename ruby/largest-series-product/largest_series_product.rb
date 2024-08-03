class Series
  attr_reader :digits

  def initialize(digits_s)
    raise ArgumentError, "Only digits can be used" if /\D/ =~ digits_s
    @digits = digits_s.chars.map(&:to_i)
  end

  def largest_product(n)
    raise ArgumentError, "Series size must be 0 <= n <= #{@digits.size}" unless (0..@digits.size) === n
    return 1 if n.zero?
    digits.each_cons(n).map { _1.reduce(1, &:*) }.max
  end
end
