class Board
  def initialize(board)
    @board = board.map { |row| row.gsub(/\s+/, '').chars }
  end

  def winner
    if wins? TOP_BOTTOM_PLAYER
      TOP_BOTTOM_PLAYER
    elsif wins? LEFT_RIGHT_PLAYER
      LEFT_RIGHT_PLAYER
    else
      ''
    end
  end

  private

  Pt = Struct.new(:x, :y) do
    def +(pt)
      Pt.new(x + pt.x, y + pt.y)
    end
  end

  TOP_BOTTOM_PLAYER = 'O'
  LEFT_RIGHT_PLAYER = 'X'
  NEIGHBOUR_DISPLACEMENTS = [
    Pt.new(0, -1),
    Pt.new(1, -1),
    Pt.new(-1, 0),
    Pt.new(1, 0),
    Pt.new(-1, 1),
    Pt.new(0, 1)
  ]

  attr_reader :board

  def [](pt)
    board[pt.y][pt.x]
  end

  def starting_points_1(player)
    case player
    when TOP_BOTTOM_PLAYER
      pts = (0...board.first.size).map { |x| Pt.new(x, 0) }
    when LEFT_RIGHT_PLAYER
      pts = (0...board.size).map { |y| Pt.new(0, y) }
    end
  end

  def starting_points_2(player)
    case player
    when TOP_BOTTOM_PLAYER
      pts = (0...board.first.size).map { |x| Pt.new(x, board.size - 1) }
    when LEFT_RIGHT_PLAYER
      pts = (0...board.size).map { |y| Pt.new(board.first.size - 1, y) }
    end
  end

  def neighbours(pt)
    NEIGHBOUR_DISPLACEMENTS.map do |disp|
      pt + disp
    end.filter do |n|
      (0...board.first.size) === n.x && (0...board.size) === n.y
    end
  end

  def paths(player, path)
    p = neighbours(path.last).filter do |n|
      self[n] == player && !path.include?(n)
    end.flat_map do |n|
      paths(player, path + [n])
    end
    p.empty? ? [path] : p
  end

  def wins?(player)
    starting_pts = [starting_points_1(player), starting_points_2(player)]
    starting_pts.map.with_index do |sps, sps_i|
      other_sps = starting_pts[sps_i - 1]
      sps.filter do |sp|
        self[sp] == player
      end.any? do |sp|
        paths(player, [sp]).any? do |path|
          path.any? do |pt|
            (player == TOP_BOTTOM_PLAYER && pt.y == other_sps.first.y) ||
            (player == LEFT_RIGHT_PLAYER && pt.x == other_sps.first.x)
          end
        end
      end
    end.any?
  end
end
