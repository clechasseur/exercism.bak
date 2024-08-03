module Hamming
  def self.compute(first, second)
    raise ArgumentError, "Strands need to be of equal length" unless first.length == second.length
    first.chars.zip(second.chars).count { |a, b| a != b }
  end
end
