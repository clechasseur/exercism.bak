class Triangle
  attr_reader :rows

  def initialize(rows)
    @rows = rows_seq.take(rows)
  end

  private

  def rows_seq
    Enumerator.produce([1]) do |prev|
      [1, *prev.each_cons(2).map(&:sum), 1]
    end
  end
end
