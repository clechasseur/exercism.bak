object SumOfMultiples {
    fun sum(multiplesOf: Set<Int>, upTo: Int): Int
        = (1 until upTo).filter { multiplesOf.multipleOfAny(it) }.sum()

    private fun Iterable<Int>.multipleOfAny(num: Int): Boolean = this.any { num % it == 0 }
}
