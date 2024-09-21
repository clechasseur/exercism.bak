class Matrix
  attr_reader :rows

  def initialize(input)
    @rows = input.split("\n").map do |row|
      row.split(" ").map { |e| e.to_i }
    end
  end

  def columns
    (0...rows.first.length).map do |y|
      rows.map { |row| row[y] }
    end
  end
end
