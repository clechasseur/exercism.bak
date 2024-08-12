module ArmstrongNumbers
  def self.include?(n)
    n == n.digits.sum { |d| d ** n.digits.size }
  end
end
