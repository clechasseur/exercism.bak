class Matrix
  attr_reader :rows, :columns

  def initialize(input)
    @rows = input.lines.map { |line| line.strip.split(" ").map(&:to_i) }
    @columns = @rows.transpose
  end

  def saddle_points
    (0...rows.size).flat_map do |row|
      (0...columns.size).map do |column|
        pt = [row, column]
        saddle_point?(pt) ? pt : nil
      end
    end.compact
  end

  private

  def saddle_point?(pt)
    value = rows[pt[0]][pt[1]]
    rows[pt[0]].all? { |e| e <= value } && columns[pt[1]].all? { |e| e >= value }
  end
end
