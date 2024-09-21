module OcrNumbers
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
    input.lines.map(&:chomp).each_slice(4).map do |lines|
      raise ArgumentError, "Invalid number of lines" unless lines.size == 4
      line_slices = lines.map do |line|
        line.chars.each_slice(3).to_a
      end
      num_digits = line_slices.first.size
      (0...num_digits).map do |n|
        digit_slices = line_slices.map { |line| line[n].join }
        raise ArgumentError, "Invalid number of columns" unless digit_slices.first.length == 3
        i = NUMBERS.index(digit_slices)
        i.nil? ? "?" : i.to_s
      end.join
    end.join(",")
  end
end
