module Minesweeper
  def self.annotate(board)
    board.map.with_index do |line, y|
      line.chars.map.with_index do |c, x|
        next c unless c == " "
        mines = neighbour_mines(board, Pt.new(x, y))
        mines.zero? ? " " : mines.to_s
      end.join
    end
  end

  private

  Pt = Struct.new(:x, :y)

  def self.neighbour_mines(board, pt)
    neighbour_points(pt, board).count { |npt| board[npt.y][npt.x] == "*" }
  end

  def self.neighbour_points(pt, board)
    ((pt.x - 1)..(pt.x + 1)).flat_map do |x|
      ((pt.y - 1)..(pt.y + 1)).flat_map do |y|
        if (0...(board[0].length)).include?(x) && (0...(board.length)).include?(y) && (x != pt.x || y != pt.y)
          [Pt.new(x, y)]
        else
          []
        end
      end
    end
  end
end
