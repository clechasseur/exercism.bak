module OcrNumbers
  LINES_PER_DIGIT = 4
  COLUMNS_PER_DIGIT = 3
  NUMBERS = [
    [" _ ",
     "| |",
     "|_|",
     "   "],
    ["   ",
     "  |",
     "  |",
     "   "],
    [" _ ",
     " _|",
     "|_ ",
     "   "],
    [" _ ",
     " _|",
     " _|",
     "   "],
    ["   ",
     "|_|",
     "  |",
     "   "],
    [" _ ",
     "|_ ",
     " _|",
     "   "],
    [" _ ",
     "|_ ",
     "|_|",
     "   "],
    [" _ ",
     "  |",
     "  |",
     "   "],
    [" _ ",
     "|_|",
     "|_|",
     "   "],
    [" _ ",
     "|_|",
     " _|",
     "   "]
  ]

  def self.convert(input)
    input.lines.map(&:chomp).each_slice(LINES_PER_DIGIT).map do |lines|
      raise ArgumentError, "Invalid number of lines" unless lines.size == LINES_PER_DIGIT
      line_slices = lines.map do |line|
        line.chars.each_slice(COLUMNS_PER_DIGIT).to_a
      end
      (0...line_slices.first.size).map do |n|
        digit_slices = line_slices.map { |line| line[n].join }
        raise ArgumentError, "Invalid number of columns" unless digit_slices.first.length == COLUMNS_PER_DIGIT
        i = NUMBERS.index(digit_slices)
        i.nil? ? "?" : i.to_s
      end.join
    end.join(",")
  end
end
