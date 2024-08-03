class Robot(
    startPosition: GridPosition = GridPosition(0, 0),
    startOrientation: Orientation = Orientation.NORTH
) {
    var gridPosition: GridPosition = startPosition
        private set
    var orientation: Orientation = startOrientation
        private set

    fun advance() {
        gridPosition = gridPosition.move(orientation)
    }
    fun turnLeft() {
        orientation = orientation.left
    }
    fun turnRight() {
        orientation = orientation.right
    }

    fun simulate(move: Char) = when (move) {
        'A' -> advance()
        'L' -> turnLeft()
        'R' -> turnRight()
        else -> error("Invalid move: $move")
    }
    fun simulate(moves: String) {
        for (move in moves) {
            simulate(move)
        }
    }
}
