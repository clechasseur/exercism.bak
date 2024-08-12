class Squares(val num: Int) {
    fun sumOfSquares() = (1..num).sumBy { it.sqr() }
    fun squareOfSum() = (1..num).sum().sqr()
    fun difference() = squareOfSum() - sumOfSquares()

    private fun Int.sqr() = this * this
}
