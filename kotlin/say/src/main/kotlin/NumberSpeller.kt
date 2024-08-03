class NumberSpeller {
    companion object {
        private val FROM_ONE = listOf("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                                      "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
        private val FROM_TWENTY = listOf("", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
        private val SCALES_REVERSED = listOf("", " thousand", " million", " billion")
    }

    fun say(num: Long): String {
        require(num >= 0) { "Input must be non-negative" }
        require(num < 1_000_000_000_000) { "Input must be less than 1000000000000" }

        val thousandsReversed = breakInThousandsReversed(num)
        val finalReversed = mutableListOf<String>()
        for ((scaleIdx, thousands) in thousandsReversed.withIndex()) {
            if (thousands != 0) {
                finalReversed.add("${sayThousands(thousands)}${SCALES_REVERSED[scaleIdx]}")
            }
        }
        if (finalReversed.isEmpty()) {
            finalReversed.add(FROM_ONE[0])
        }
        
        return finalReversed.reversed().joinToString(" ")
    }

    fun sayThousands(num: Int): String {
        require(num < 1_000)
        val sb = StringBuilder()

        val hundreds = num / 100
        if (hundreds > 0) {
            sb.append("${FROM_ONE[hundreds]} hundred ")
        }

        val below100 = num % 100
        if (below100 >= 20) {
            sb.append("${FROM_TWENTY[below100 / 10]}")
            val remainder = below100 % 10
            if (remainder != 0) {
                sb.append("-${FROM_ONE[remainder]}")
            }
        } else if (below100 != 0) {
            sb.append(FROM_ONE[below100])
        }

        return sb.toString().trim()
    }

    fun breakInThousandsReversed(num: Long): List<Int> {
        val groups = mutableListOf<Int>()
        var remain = num
        while (remain != 0L) {
            groups.add((remain % 1_000L).toInt())
            remain /= 1_000L
        }
        if (groups.isEmpty()) {
            groups.add(0)
        }
        return groups
    }
}
