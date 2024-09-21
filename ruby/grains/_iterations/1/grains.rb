module Grains
  def self.square(n)
    raise ArgumentError, "n must be in 1..64" unless (1..64) === n
    2 ** (n - 1)
  end

  def self.total
    2 ** 64 - 1
  end
end
