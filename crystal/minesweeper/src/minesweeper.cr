class Minesweeper
  # An (almost) straight port of my Ruby solution:
  # https://exercism.org/tracks/ruby/exercises/minesweeper/solutions/clechasseur

  def initialize(@board : Array(String)); end

  def annotate
    @board.map_with_index do |line, y|
      line.chars.map_with_index do |c, x|
        next c unless c == ' '

        mines = neighbour_mines([x, y])
        mines.zero? ? ' ' : mines.to_s[0]
      end.join
    end.to_a
  end

  private def neighbour_mines(pt)
    neighbour_points(pt).count { |npt| @board[npt[1]][npt[0]] == '*' }
  end

  private def neighbour_points(pt)
    ((pt[0] - 1)..(pt[0] + 1)).flat_map do |x|
      ((pt[1] - 1)..(pt[1] + 1)).flat_map do |y|
        if (0...@board[0].size).includes?(x) && (0...@board.size).includes?(y) && (x != pt[0] || y != pt[1])
          [[x, y]]
        else
          [] of Array(Int32)
        end
      end
    end
  end
end
