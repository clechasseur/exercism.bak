module OcrNumbers
  def self.convert(input)
    input.lines.map(&:chomp).each_slice(LINES_PER_GROUP).map do |lines|
      convert_group(lines)
    end.join(",")
  end

  private

  LINES_PER_GROUP = 4
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

  def self.convert_group(lines)
    raise ArgumentError, "Invalid number of lines" unless lines.size == LINES_PER_GROUP
    lines.map do |line|
      line.chars.each_slice(COLUMNS_PER_DIGIT).to_a
    end.transpose.map do |digit|
      convert_digit(digit.map(&:join))
    end.join
  end

  def self.convert_digit(digit_lines)
    raise ArgumentError, "Invalid number of columns" unless digit_lines.all? { _1.length == COLUMNS_PER_DIGIT }
    i = NUMBERS.index(digit_lines)
    i.nil? ? "?" : i.to_s
  end
end
