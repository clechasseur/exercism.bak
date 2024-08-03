class Matrix(matrixAsString: String) {
    private val rows = matrixAsString.lineSequence().toList().toInts()
    private val columns = rows[0].indices.map { colIdx -> rows.map { row -> row[colIdx] } }

    fun column(colNr: Int): List<Int> = columns[colNr - 1]

    fun row(rowNr: Int): List<Int> = rows[rowNr - 1]

    private fun List<String>.toInts() = map { s -> s.trim().split(Regex("\\s+")).map { it.toInt() } }
}
