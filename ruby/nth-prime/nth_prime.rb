module Prime
  def self.nth(n)
    raise ArgumentError, "n must be positive" unless n.positive?
    return 2 if n == 1

    n -= 1
    x = 3
    loop do
      n -= 1 if prime? x
      return x if n.zero?
      x += 2
    end
  end

  def self.prime?(x)
    (2..Math.sqrt(x)).none? { |d| x % d == 0 }
  end
end
