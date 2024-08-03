import kotlin.math.pow

object ResistorColorDuo {
    fun value(vararg colors: Color)
            = colors.take(2).reversed().mapIndexed { i, c -> c.ordinal * 10.pow(i) }.sum()

    private fun Int.pow(exponent: Int) = toDouble().pow(exponent).toInt()
}
