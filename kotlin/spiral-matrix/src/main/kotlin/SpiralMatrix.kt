object SpiralMatrix {
    fun ofSize(size: Int): Array<IntArray> = when (size) {
        0 -> emptyArray<IntArray>()
        1 -> arrayOf(intArrayOf(1))
        else -> {
            val matrix = Array(size) { IntArray(size) { 0 } }
            
            var coord = Coord(0, 0)
            var dir = Direction.RIGHT
            var num = 1
            do {
                matrix[coord.row][coord.col] = num++
                val nextCoord = coord.move(dir)
                if (nextCoord.outOfBounds(size) || matrix[nextCoord.row][nextCoord.col] != 0) {
                    dir = dir.next
                    coord = coord.move(dir)
                } else {
                    coord = nextCoord
                }
            } while (!coord.outOfBounds(size) && matrix[coord.row][coord.col] == 0)

            matrix
        }
    }
}

private enum class Direction {
    UP, DOWN, LEFT, RIGHT;

    val next: Direction get() = when (this) {
        UP -> RIGHT
        LEFT -> UP
        DOWN -> LEFT
        RIGHT -> DOWN
    }
}

private data class Coord(val row: Int, val col: Int) {
    fun move(dir: Direction): Coord = when (dir) {
        Direction.UP -> Coord(row - 1, col)
        Direction.DOWN -> Coord(row + 1, col)
        Direction.LEFT -> Coord(row, col - 1)
        Direction.RIGHT -> Coord(row, col + 1)
    }

    fun outOfBounds(size: Int) = row < 0 || row >= size || col < 0 || col >= size
}
