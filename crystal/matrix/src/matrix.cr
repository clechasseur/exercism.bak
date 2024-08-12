class Matrix
  @rows : Array(Array(Int32))
  @cols : Array(Array(Int32))

  def initialize(matrix : String)
    @rows = matrix.lines.map(&.split.map(&.to_i))
    @cols = @rows.transpose
  end

  def row(i)
    @rows[i - 1]
  end

  def column(i)
    @cols[i - 1]
  end
end
