module PerfectNumber
  def self.classify(n)
    raise RuntimeError, "n must be positive" unless n.positive?
    
    aliquot_sum = factors(n).reject { |x| x == n }.sum

    return "perfect" if aliquot_sum == n
    return "abundant" if aliquot_sum > n
    "deficient"
  end

  def self.factors(n)
    (1..Math.sqrt(n)).filter { |d| n % d == 0 }.flat_map { |f| [f, n / f] }.uniq
  end
end
