import java.lang.IllegalArgumentException
import kotlin.math.abs
import kotlin.math.ceil

class AffineCipher(private val a: Int, private val b: Int) {
    companion object {
        private const val m = 26

        fun encode(input: String, a: Int, b: Int) = AffineCipher(a, b).encode(input)
        fun decode(input: String, a: Int, b: Int) = AffineCipher(a, b).decode(input)
    }

    private val mmi = (generateSequence(m + 1) { it + m }
            .take(50)   // Arbitrary limit of mmi
            .filter { it % a == 0 }
            .firstOrNull() ?: throw IllegalArgumentException("a and m must be coprime.")) / a

    fun encode(input: String) = input.asSequence()
            .filter { it.isLetterOrDigit() }
            .map { it.toLowerCase() }
            .map { when(it) {
                in 'a'..'z' -> 'a' + ((a * (it - 'a') + b) % m)
                else -> it
            } }
            .chunked(5) { it.joinToString("") }
            .joinToString(" ")

    fun decode(input: String) = input.asSequence()
            .filter { it.isLetterOrDigit() }
            .map { when(it) {
                in 'a'..'z' -> 'a' + (mmi * (it - 'a' - b)).constraintToM()
                else -> it
            } }
            .joinToString("")

    private fun Int.constraintToM() = when {
        this < 0 -> this + (ceil(abs(this).toDouble() / m).toInt() * m)
        else -> this % m
    }
}
