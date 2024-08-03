class Series(val digitsStr: String) {
    init {
        require(digitsStr.all { it.isDigit() })
    }

    fun getLargestProduct(len: Int): Int {
        require(len >= 0)
        require(len <= digitsStr.length)
        return digitsStr.allSubstringsOfLen(len)
                        .map { it.fold(1) { acc, c -> acc * (c - '0') } }
                        .max() ?: 1
    }

    private fun String.allSubstringsOfLen(len: Int): List<String>
        = (0..(this.length - len)).map { this.substring(it until it + len) }
}
