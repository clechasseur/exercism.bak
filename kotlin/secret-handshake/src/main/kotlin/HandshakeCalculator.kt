object HandshakeCalculator {
    fun calculateHandshake(num: Int): List<Signal>
        = SIGNAL_VALUES.filterKeys { num.hasBits(it) }.values.reversedIf(num.hasBits(0b10000))

    private fun Int.hasBits(bits: Int): Boolean
        = (this and bits) == bits

    private fun <T> Iterable<T>.reversedIf(cond: Boolean): List<T>
        = if (cond) this.reversed() else this.toList()

    private val SIGNAL_VALUES: Map<Int, Signal> by lazy {
        sortedMapOf(
            0b0001 to Signal.WINK,
            0b0010 to Signal.DOUBLE_BLINK,
            0b0100 to Signal.CLOSE_YOUR_EYES,
            0b1000 to Signal.JUMP
        )
    }
}
