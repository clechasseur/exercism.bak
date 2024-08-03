object CollatzCalculator {
    fun computeStepCount(num: Int): Int {
        require(num >= 1) { "Only natural numbers are allowed" }
        return generateSequence(num) { n -> if (n % 2 == 0) n / 2 else n * 3 + 1 }.indexOf(1)
    }
}
