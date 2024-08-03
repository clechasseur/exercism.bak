object WordCount {
    private val WORD = """\w(?!\w)|\w[\w']*\w""".toRegex()

    fun phrase(input: String): Map<String, Int> {
        return WORD.findAll(input.toLowerCase()).groupingBy { it.value }.eachCount()
    }
}
