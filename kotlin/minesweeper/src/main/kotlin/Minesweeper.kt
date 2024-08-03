class MinesweeperBoard(private val inputBoard: List<String>) {
    fun withNumbers(): List<String> {
        val intBoard: List<MutableList<Int>> = inputBoard.map { row ->
            row.map { c -> c.toMinesweeper() }.toMutableList()
        }
        val dimensions = Dimensions(intBoard.size, intBoard.firstOrNull()?.size ?: 0)

        Points.forMatrix(dimensions).forEach { pt ->
            if (intBoard[pt.row][pt.col] == -1) {
                Points.neighboursOf(pt, dimensions).forEach { incPt ->
                    val incVal = intBoard[incPt.row][incPt.col]
                    if (incVal != -1) {
                        intBoard[incPt.row][incPt.col] = incVal + 1
                    }
                }
            }
        }

        return intBoard.map { row ->
            row.map { i -> i.toMinesweeper() }.joinToString("")
        }
    }

    private fun Char.toMinesweeper() = when (this) {
        '*' -> -1
        ' ' -> 0
        else -> this - '0'
    }

    private fun Int.toMinesweeper() = when (this) {
        -1 -> '*'
        0 -> ' '
        else -> '0' + this
    }
}

private data class Dimensions(val numRows: Int, val numCols: Int) {
    val solid: Boolean
        get() = numRows > 0 && numCols > 0
}

private data class Point(val row: Int, val col: Int)

private object Points {
    fun forMatrix(dimensions: Dimensions): Sequence<Point> {
        return generateSequence({
            if (dimensions.solid) Point(0, 0) else null
        }, { prev ->
            var next: Point? = Point(prev.row, prev.col + 1)
            if (next!!.col == dimensions.numCols) {
                next = Point(prev.row + 1, 0)
                if (next.row == dimensions.numRows) {
                    next = null
                }
            }
            next
        })
    }

    fun neighboursOf(pt: Point, dimensions: Dimensions): List<Point> {
        val neighbours = mutableListOf<Point>()
        for (row in (pt.row - 1)..(pt.row + 1)) {
            for (col in (pt.col - 1)..(pt.col + 1)) {
                if (row >= 0 && row < dimensions.numRows &&
                    col >= 0 && col < dimensions.numCols &&
                    (row != pt.row || col != pt.col)
                ) {
                    neighbours.add(Point(row, col))
                }
            }
        }
        return neighbours
    }
}
