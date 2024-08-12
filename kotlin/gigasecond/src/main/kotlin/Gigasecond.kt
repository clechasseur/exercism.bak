import java.time.LocalDate
import java.time.LocalDateTime

class Gigasecond(val initialDateTime: LocalDateTime) {
    constructor(initialDate: LocalDate) : this(initialDate.atStartOfDay())

    val date = initialDateTime.plusSeconds(1E9.toLong())
}
