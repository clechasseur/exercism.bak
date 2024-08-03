object PascalsTriangle {
    fun computeTriangle(numRows: Int): List<List<Int>> {
        require(numRows >= 0) { "Rows can't be negative!" }
        return generateSequence(listOf(1)) { nextRow(it) }.take(numRows).toList()
    }

    fun nextRow(prevRow: List<Int>): List<Int>
        = listOf(1).plus((1 until prevRow.size).map { prevRow[it - 1] + prevRow[it] }).plus(1)
}
