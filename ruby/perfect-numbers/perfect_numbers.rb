module PerfectNumber
  def self.classify(n)
    raise RuntimeError, "n must be positive" unless n.positive?
    
    aliquot_sum = (1...n).filter { |d| n % d == 0 }.sum

    case aliquot_sum
    when n
      "perfect"
    when 0...n
      "deficient"
    else
      "abundant"
    end
  end
end
