class Matrix(val values: List<List<Int>>) {
    val size: Int get() = values[0].size
    fun at(row: Int, col: Int): Int = values[row][col]

    val saddlePoints: Set<MatrixCoordinate> get()
        = (0..(size - 1)).flatMap { row -> (0..(size - 1)).map { col -> MatrixCoordinate(row, col) } }
                         .filter { isSaddlePoint(it) }
                         .toSet()

    private fun isSaddlePoint(coord: MatrixCoordinate): Boolean {
        val value = at(coord.row, coord.col)
        return (0..(size - 1)).all { at(coord.row, it) <= value } &&
               (0..(size - 1)).all { at(it, coord.col) >= value }
    }
}
