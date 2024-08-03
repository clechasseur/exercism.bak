class SumOfMultiples
  attr_reader :multiples

  def initialize(*multiples)
    @multiples = multiples
  end

  def to(n)
    (1...n).filter do |x|
      multiples.any? { |m| x % m == 0 }
    end.sum
  end
end
