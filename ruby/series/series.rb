class Series
  def initialize(input)
    @input = input
  end

  def slices(length)
    raise ArgumentError, "Invalid length: #{length}" unless (1..@input.length) === length
    @input.chars.each_cons(length).map(&:join)
  end
end
