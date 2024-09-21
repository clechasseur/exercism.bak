class DiamondPrinter {
    fun printToList(letter: Char): List<String> = when(letter) {
        'A' -> listOf("A")
        else -> {
            val diamondSize = 1 + (letter - 'A') * 2
            ('A'..letter).plus((letter - 1) downTo 'A').map { diamondLine(it, diamondSize) }
        }
    }

    private fun diamondLine(letter: Char, diamondSize: Int): String = when(letter) {
        'A' -> {
            val outerWhitespace = getWhitespace((diamondSize - 1) / 2)
             "${outerWhitespace}${letter}${outerWhitespace}"
        }
        else -> {
            val innerSize = (letter - 'A') * 2 - 1
            val outerSize = (diamondSize - innerSize - 2) / 2
            val innerWhitespace = getWhitespace(innerSize)
            val outerWhitespace = getWhitespace(outerSize)
            "${outerWhitespace}${letter}${innerWhitespace}${letter}${outerWhitespace}"
        }
    }

    private fun getWhitespace(size: Int): String = (1..size).map { ' ' }.joinToString("")
}
