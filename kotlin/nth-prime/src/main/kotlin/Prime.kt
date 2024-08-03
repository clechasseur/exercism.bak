object Prime {
    fun nth(index: Int): Int {
        require(index > 0) { "There is no zeroth prime." }
        return generateSequence(2) { it + 1 }.filter { it.isPrime() }.elementAt(index - 1)
    }

    private fun Int.isPrime() = (2..(this / 2)).all { this % it != 0 }
}
