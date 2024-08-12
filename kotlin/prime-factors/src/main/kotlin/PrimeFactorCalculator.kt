object PrimeFactorCalculator {
    fun primeFactors(num: Int): List<Int> = primeFactors(num.toLong()).map { it.toInt() }
    fun primeFactors(num: Long): List<Long> {
        require(num > 0L)
        val factors = mutableListOf<Long>()
        var cur = num
        var factor = 2L
        while (cur != 1L) {
            if ((cur % factor) == 0L) {
                factors.add(factor)
                cur /= factor
            } else {
                ++factor
            }
        }
        return factors
    }
}
