module Transpose
  def self.transpose(input)
    return "" if input.empty?
    matrix = input.lines.map { |line| line.chomp.chars }
    longest = matrix.max_by(&:length).length
    (0...longest).map do |x|
      matrix.map do |line|
        line[x]
      end.reverse.drop_while(&:nil?).reverse.map do |c|
        c.nil? ? " " : c
      end.join
    end.join("\n")
  end
end
