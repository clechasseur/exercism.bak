import kotlin.math.pow

object ArmstrongNumber {
    fun check(input: Int): Boolean {
        val inputStr = input.toString()
        val numDigits = inputStr.length
        return inputStr.map { it.toString().toDouble().pow(numDigits).toInt() }.sum() == input
    }
}
