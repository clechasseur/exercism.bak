object ResistorColor {
    private val COLORS = listOf("black", "brown", "red", "orange", "yellow", "green", "blue", "violet", "grey", "white")

    fun colorCode(input: String) = COLORS.indexOf(input)

    fun colors() = COLORS
}
