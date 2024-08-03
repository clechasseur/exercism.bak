import kotlin.math.pow

object Wordy {
    private val ops = mapOf<Regex, (Int, Int) -> Int>(
            Regex("""plus (-?\d+)""") to { x, y -> x + y },
            Regex("""minus (-?\d+)""") to { x, y -> x - y },
            Regex("""multiplied by (-?\d+)""") to { x, y -> x * y },
            Regex("""divided by (-?\d+)""") to { x, y -> x / y.notZero() },
            Regex("""raised to the (\d+)(?:st|nd|rd|th) power""") to { x, y -> x.toDouble().pow(y).toInt() }
    )
    private val opsExpression = " (${ops.keys.joinToString("|") { it.pattern }})"
    private val inputRegex = Regex("What is (-?\\d+)(($opsExpression)*)\\?")
    private val opsRegex = Regex(opsExpression)

    fun answer(input: String): Int {
        val wholeMatch = inputRegex.matchEntire(input)
        require(wholeMatch != null) { "invalid input" }
        var value = wholeMatch.groupValues[1].toInt()
        opsRegex.findAll(wholeMatch.groupValues[2]).forEach {
            exprMatch -> value = execute(exprMatch.groupValues[1], value)
        }
        return value
    }

    private fun execute(input: String, x: Int): Int {
        return ops.mapNotNull {
            (re, op) -> when (val match = re.matchEntire(input)) {
                null -> null
                else -> op(x, match.groupValues[1].toInt())
            }
        }.first()
    }

    private fun Int.notZero() = apply { require(this != 0) { "non-zero value required" } }
}
