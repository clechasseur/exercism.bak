module ArmstrongNumbers
  def self.include?(n)
    digits = n.to_s.chars
    n == digits.sum { |d| d.to_i ** digits.size }
  end
end
