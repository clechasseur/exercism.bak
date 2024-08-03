class BaseConverter(val inBase: Int, val inDigits: IntArray) {
    init {
        require(inBase >= 2) { "Bases must be at least 2." }
        require(!inDigits.isEmpty()) { "You must supply at least one digit." }
        require(inDigits.first() != 0 || inDigits.size == 1) { "Digits may not contain leading zeros." }
        require(inDigits.all { it >= 0 }) { "Digits may not be negative." }
        require(inDigits.all { it < inBase }) { "All digits must be strictly less than the base." }
    }

    val toBase10: Int by lazy {
        powersOf(inBase).zip(inDigits.reversed().asSequence()) { a, b -> a * b }.sum()
    }

    fun convertToBase(outBase: Int): IntArray {
        require(outBase >= 2) { "Bases must be at least 2." }
        
        val outDigits = mutableListOf<Int>()
        var num = toBase10
        while (num != 0) {
            outDigits.add(num % outBase)
            num = num / outBase
        }
        if (outDigits.isEmpty()) {
            outDigits.add(0)
        }

        return outDigits.reversed().toIntArray()
    }

    private fun powersOf(num: Int): Sequence<Int> = generateSequence(1) { it * num }
}
