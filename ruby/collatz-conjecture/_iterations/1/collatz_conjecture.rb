module CollatzConjecture
  def self.steps(n)
    raise ArgumentError, "n must be > 0" unless n > 0
    Enumerator.produce(n) { |p| p.even? ? p / 2 : 3 * p + 1 }.take_while { |p| p != 1 }.count
  end
end
