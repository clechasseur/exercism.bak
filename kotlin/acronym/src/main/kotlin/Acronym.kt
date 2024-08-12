object Acronym {
    private val wordRegex = Regex("[A-Z]+")

    fun generate(sen: String): String =
        sen.toUpperCase().replace(wordRegex) { it.value.get(0).toString() }.filter { it in 'A'..'Z' }
}
