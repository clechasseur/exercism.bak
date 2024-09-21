data class Clock(private var _hours: Int, private var _minutes: Int) {
    init {
        adjust()
    }

    val hours: Int get() = _hours
    val minutes: Int get() = _minutes

    fun add(minutesDelta: Int) {
        _minutes += minutesDelta
        adjust()
    }

    fun subtract(minutesDelta: Int) {
        add(-minutesDelta)
    }

    override fun toString(): String {
        return "${hours.toLeadingZeroString()}:${minutes.toLeadingZeroString()}"
    }

    private fun adjust() {
        while (_minutes < 0) {
            --_hours
            _minutes += 60
        }
        while (_minutes >= 60) {
            ++_hours
            _minutes -= 60
        }
        while (_hours < 0) {
            _hours += 24
        }
        while (_hours >= 24) {
            _hours -= 24
        }
    }

    private fun Int.toLeadingZeroString(): String = if (this < 10) "0$this" else toString()
}
