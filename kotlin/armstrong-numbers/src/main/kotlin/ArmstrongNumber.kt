import kotlin.math.pow
import kotlin.math.log10
import kotlin.math.floor

object ArmstrongNumber {
    fun check(number: Int): Boolean = number == sumOfDigitsRaisedToPowerOfNumDigits(number)

    private fun sumOfDigitsRaisedToPowerOfNumDigits(number: Int): Int {
        val numDigits = number.digitCount
        var n = number
        var sum = 0.0
        while (n > 0) {
            sum += (n % 10).toDouble().pow(numDigits)
            n /= 10
        }
        return sum.toInt()
    }

    private val Int.digitCount: Int
        get() = floor(log10(toDouble()) + 1.0).toInt()
}