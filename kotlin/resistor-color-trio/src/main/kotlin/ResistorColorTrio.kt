import kotlin.math.pow

object ResistorColorTrio {
    fun text(vararg input: Color): String {
        var value = input
                .take(2)
                .reversed()
                .mapIndexed { i, c -> c.ordinal * 10.pow(i) }
                .sum() * 10.pow(input[2].ordinal)
        var unit = Unit.OHMS
        while (value > 1000 && value % 1000 == 0) {
            value /= 1000;
            unit = unit.next()
        }
        return "$value $unit"
    }

    private fun Int.pow(exponent: Int) = toDouble().pow(exponent).toInt()
}
