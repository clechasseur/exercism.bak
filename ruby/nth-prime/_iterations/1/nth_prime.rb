module Prime
  def self.nth(n)
    raise ArgumentError, "n must be positive" unless n.positive?

    primes = []
    x = 2
    while primes.size < n
      primes << x if primes.none? { |p| x % p == 0 }
      x += 1
    end
    primes.last
  end
end
