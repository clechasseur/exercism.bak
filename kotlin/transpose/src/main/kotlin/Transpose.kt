import kotlin.math.min

object Transpose {
    fun transpose(input: List<String>): List<String> {
        val rowLen = input.map { it.length }.max() ?: 0
        val transposed = (0 until rowLen).map {
            colIdx -> input.map { it.getOrElse(colIdx) { ' ' } }.joinToString("")
        }.toMutableList()
        var toTrim = Int.MAX_VALUE
        for (i in transposed.indices.reversed()) {
            val line = transposed[i]
            val canTrim = line.reversed().takeWhile { it == ' ' }.mapIndexed{ idx, _ -> idx < toTrim }.count { it }
            toTrim = min(toTrim, canTrim)
            if (toTrim == 0) {
                break
            }
            transposed[i] = line.substring(0, line.length - toTrim)
        }
        return transposed
    }
}
