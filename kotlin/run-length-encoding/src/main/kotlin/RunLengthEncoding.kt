object RunLengthEncoding {
    private val encodeRegex = Regex("""([A-Za-z ])\1+""")
    private val decodeRegex = Regex("""(\d+)([A-Za-z ])""")

    fun encode(input: String) = encodeRegex.replace(input) { it.value.length.toString() + it.groupValues[1] }

    fun decode(input: String) = decodeRegex.replace(input) { it.groupValues[2].repeat(it.groupValues[1].toInt()) }
}
