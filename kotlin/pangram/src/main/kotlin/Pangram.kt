object Pangram {
    fun isPangram(sen: String): Boolean =
        sen.toLowerCase().toList().filter { it.isLetter() }.distinct().size == 26
}
