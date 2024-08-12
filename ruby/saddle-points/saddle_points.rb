class Matrix
  attr_reader :rows, :columns

  def initialize(input)
    @rows = input.lines.map { |line| line.strip.split(" ").map(&:to_i) }
    @columns = @rows.transpose
  end

  def saddle_points
    rows.flat_map.with_index do |row, row_num|
      row.flat_map.with_index do |e, col_num|
        saddle_point = row.all? { |n| n <= e } && columns[col_num].all? { |n| n >= e }
        saddle_point ? [[row_num, col_num]] : []
      end
    end
  end
end
