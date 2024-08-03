object Atbash {
    fun encode(str: String): String = str.atbash().byGroupsOf(5)
    fun decode(str: String): String = str.atbash()

    private fun Char.atbash() = if (this.isLetter()) 'a' + ('z' - this) else this
    private fun String.atbash()
        = this.filter { it.isLetterOrDigit() }.toLowerCase().map { it.atbash() }.joinToString("")
    private fun String.byGroupsOf(num: Int) = this.chunked(num).joinToString(" ")
}
