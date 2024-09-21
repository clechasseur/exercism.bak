class Series
  def initialize(input)
    @input = input
  end

  def slices(length)
    raise ArgumentError, "Invalid length: #{length}" unless (1..@input.length) === length
    (0..(@input.length - length)).map { |x| @input[x, length] }
  end
end
