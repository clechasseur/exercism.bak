
enum class Classification {
    DEFICIENT, PERFECT, ABUNDANT
}

fun classify(naturalNumber: Int): Classification {
    if (naturalNumber < 1) {
        throw IllegalArgumentException("Natural numbers are >= 1.")
    }
    val alSum = aliquotSum(naturalNumber)
    return when {
        alSum < naturalNumber -> Classification.DEFICIENT
        alSum > naturalNumber -> Classification.ABUNDANT
        else -> Classification.PERFECT
    }
}

fun aliquotSum(num: Int): Int =  (1 until num).filter { num % it == 0 }.sum()
