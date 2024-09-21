module Board
  def self.transform(board)
    raise ArgumentError, "Board must have at least two lines" if board.size < 2
    raise ArgumentError, "Board's top is invalid" unless /^\+-+\+$/ =~ board.first
    raise ArgumentError, "Board's bottom is invalid" unless /^\+-+\+$/ =~ board.last
    raise ArgumentError, "Board contains invalid characters" unless board[1..-2].all? { |l| /^\|[ *]*\|$/ =~ l }
    raise ArgumentError, "Board lines must be of equal length" unless board.all? { |l| l.length == board.first.length }

    board.map.with_index do |line, y|
      line.chars.map.with_index do |c, x|
        if c == " "
          mines = neighbour_mines(board, Pt.new(x, y))
          mines.zero? ? " " : mines.to_s
        else
          c
        end
      end.join
    end
  end

  private

  Pt = Struct.new(:x, :y)

  def self.neighbour_mines(board, pt)
    neighbour_points(pt).count { |npt| board[npt.y][npt.x] == "*" }
  end

  def self.neighbour_points(pt)
    ((pt.x - 1)..(pt.x + 1)).flat_map do |x|
      ((pt.y - 1)..(pt.y + 1)).flat_map do |y|
        x != 0 || y != 0 ? [Pt.new(x, y)] : []
      end
    end
  end
end
