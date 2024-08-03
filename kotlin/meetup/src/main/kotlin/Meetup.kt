import java.time.DayOfWeek
import java.time.LocalDate

class Meetup(val month: Int, val year: Int) {
    fun day(dayOfWeek: DayOfWeek, schedule: MeetupSchedule): LocalDate {
        val dates = datesOfDay(dayOfWeek)
        return when (schedule) {
            MeetupSchedule.TEENTH -> dates.first { it.getDayOfMonth() in 13..19 }
            MeetupSchedule.LAST -> dates.toList().reversed().first()
            else -> dates.elementAt(schedule.offset - 1)
        }
    }

    private fun datesOfDay(dayOfWeek: DayOfWeek): Sequence<LocalDate>
        = generateSequence(firstDayOfMonth(dayOfWeek)) {
            val next = it.plusDays(7)
            if (next.getMonthValue() == month) next else null
        }

    private fun firstDayOfMonth(dayOfWeek: DayOfWeek): LocalDate {
        var date = LocalDate.of(year, month, 1)
        while (date.getDayOfWeek() != dayOfWeek) {
            date = date.plusDays(1)
        }
        return date
    }
}
