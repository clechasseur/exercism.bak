import kotlin.math.sqrt

object Darts {
    fun score(x: Int, y: Int) = score(x.toDouble(), y.toDouble())
    fun score(x: Int, y: Double) = score(x.toDouble(), y)
    fun score(x: Double, y: Int) = score(x, y.toDouble())

    fun score(x: Double, y: Double): Int {
        val dist = distance(x, y)
        return when {
            dist > 10.0 -> 0
            dist > 5.0 -> 1
            dist > 1.0 -> 5
            else -> 10
        }
    }

    private fun distance(x: Double, y: Double): Double = sqrt(x * x + y * y)
}
