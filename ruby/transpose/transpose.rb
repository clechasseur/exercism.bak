module Transpose
  def self.transpose(input)
    return "" if input.empty?
    
    lines = input.lines.map(&:chomp)
    max_length = lines.map(&:length).max
    matrix = lines.map { |line| line.ljust(max_length, "_").chars }
    matrix.transpose.map { |row| row.join.sub(/_+$/, "").gsub(/_/, " ") }.join("\n")
  end
end
