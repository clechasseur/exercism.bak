class IsbnVerifier {
    companion object {
        private val ISBN_FORMAT = """\d{9}[\dX]""".toRegex()
    }

    fun isValid(isbn: String): Boolean {
        val cleaned = isbn.filter { it != '-' }.trim()
        if (!ISBN_FORMAT.matches(cleaned)) {
            return false
        }
        return cleaned.map { it.toIsbnDigit() }
                      .mapIndexed { idx, d -> d * (10 - idx) }
                      .sum() % 11 == 0
    }

    private fun Char.toIsbnDigit() = when (this) {
        in '0'..'9' -> this - '0'
        else -> 10
    }
}
