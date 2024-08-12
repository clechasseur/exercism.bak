object RomanNumeral {
    fun value(num: Int): String
        = num.toString()
             .map { it - '0' }
             .reversed()
             .withIndex()
             .map { (idx, digit) -> valueForDigit(digit, NUMERAL_INFOS[idx]) }
             .reversed()
             .joinToString("")

    private fun valueForDigit(digit: Int, info: NumeralInfo): String = when {
        digit <  4 -> generateSequence { info.one }.take(digit).joinToString("")
        digit == 4 -> "${info.one}${info.five}"
        digit <  9 -> generateSequence(info.five) { info.one }.take(digit - 4).joinToString("")
        digit == 9 -> "${info.one}${info.ten}"
        else -> ""
    }

    private val NUMERAL_INFOS = listOf(
        NumeralInfo('I', 'V', 'X'),
        NumeralInfo('X', 'L', 'C'),
        NumeralInfo('C', 'D', 'M'),
        NumeralInfo('M', '?', '?')
    )
}

private data class NumeralInfo(val one: Char, val five: Char, val ten: Char)
