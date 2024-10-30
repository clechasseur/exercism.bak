class Minesweeper
  # An (almost) straight port of my Ruby solution:
  # https://exercism.org/tracks/ruby/exercises/minesweeper/solutions/clechasseur

  def initialize(@board : Array(String)); end

  def annotate
    @board.map_with_index do |line, y|
      line.chars.map_with_index do |c, x|
        next c unless c == ' '

        mines = neighbour_mines(Pt.new(x, y))
        mines.zero? ? ' ' : mines.to_s[0]
      end.join
    end.to_a
  end

  private struct Pt
    property x, y

    def initialize(@x : Int32, @y : Int32); end
  end

  private def neighbour_mines(pt)
    neighbour_points(pt).count { |npt| @board[npt.y][npt.x] == '*' }
  end

  private def neighbour_points(pt)
    ((pt.x - 1)..(pt.x + 1)).flat_map do |x|
      ((pt.y - 1)..(pt.y + 1)).flat_map do |y|
        if (0...@board[0].size).includes?(x) && (0...@board.size).includes?(y) && (x != pt.x || y != pt.y)
          [Pt.new(x, y)]
        else
          [] of Pt
        end
      end
    end
  end
end
