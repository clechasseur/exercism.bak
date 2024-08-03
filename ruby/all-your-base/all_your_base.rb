module BaseConverter
  def self.convert(input_base, digits, output_base)
    raise ArgumentError, "Input base must be >= 2" unless input_base >= 2
    raise ArgumentError, "Output base must be >= 2" unless output_base >= 2
    raise ArgumentError, "Digits must be >= 0" unless digits.all? { |d| d >= 0 }
    raise ArgumentError, "Digits must be < #{input_base}" unless digits.all? { |d| d < input_base }

    val = digits.reverse.map.with_index { |d, i| d * (input_base ** i) }.sum
    return [0] if val.zero?

    output = []
    while val > 0
      val, digit = val.divmod(output_base)
      output << digit
    end
    output.reverse
  end
end
