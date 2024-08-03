module PrimeFactors
  def self.of(n)
    factors = []
    d = 2
    while n > 1
      q, m = n.divmod d
      if m.zero?
        factors << d
        n = q
      else
        d += 1
      end
    end
    factors
  end
end
