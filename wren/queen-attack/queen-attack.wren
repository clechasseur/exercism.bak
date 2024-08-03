class QueenAttack {
  construct new() {
    _black = [0, 3]
    _white = [7, 3]
  }

  construct new(pieces) {
    _black = validateQueen(pieces["black"] || [0, 3])
    _white = validateQueen(pieces["white"] || [7, 3])

    if (_black[0] == _white[0] && _black[1] == _white[1]) {
      Fiber.abort("Queens cannot share the same space")
    }
  }

  black { _black }
  white { _white }

  toString {
    return (0..7).map {|row|
      return (0..7).map {|col|
        if (row == black[0] && col == black[1]) {
          return "B"
        } else if (row == white[0] && col == white[1]) {
          return "W"
        }
        return "_"
      }.join(" ")
    }.join("\n")
  }

  canAttack {
    return black[0] == white[0] ||
      black[1] == white[1] ||
      (black[0] - white[0]).abs == (black[1] - white[1]).abs
  }

  validateQueen(pos) {
    if (pos.any {|coord| coord < 0 || coord > 7 }) {
      Fiber.abort("Queen must be placed on the board")
    }

    return pos
  }
}
