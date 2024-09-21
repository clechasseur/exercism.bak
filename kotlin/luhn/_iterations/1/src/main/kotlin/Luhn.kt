object Luhn {
    fun isValid(str: String): Boolean {
        val strippedStr = str.filter { !it.isWhitespace() }
        return when {
            strippedStr.length <= 1 -> false
            strippedStr.any { !it.isDigit() } -> false
            else -> strippedStr.map { it - '0' }.isLuhn()
        }
    }

    private fun Iterable<Int>.isLuhn(): Boolean
        = this.reversed()
              .mapIndexed { idx, num -> if (idx % 2 != 0) num * 2 else num }
              .map { if (it > 9) it - 9 else it }
              .sum() % 10 == 0
}
