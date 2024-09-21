import java.time.DayOfWeek;
import java.time.LocalDate;

class Meetup {
    private final LocalDate base;

    Meetup(final int month, final int year) {
        base = LocalDate.of(year, month, 1);
    }

    LocalDate day(DayOfWeek dayOfWeek, MeetupSchedule schedule) {
        LocalDate output = base;
        while (output.getDayOfWeek() != dayOfWeek) {
            output = output.plusDays(1);
        }
        return day(output, schedule);
    }

    private LocalDate day(LocalDate base, MeetupSchedule schedule) {
        switch (schedule) {
            case FIRST: {
                return base;
            }
            case SECOND:
            case THIRD:
            case FOURTH: {
                return day(base.plusDays(7), schedule.previous());
            }
            case LAST: {
                LocalDate next = base.plusDays(7);
                if (next.getMonth() != base.getMonth()) {
                    return base;
                }
                return day(next, MeetupSchedule.LAST);
            }
            case TEENTH: {
                if (base.getDayOfMonth() >= 13) {
                    return base;
                }
                return day(base.plusDays(7), MeetupSchedule.TEENTH);
            }
        }
        throw new IllegalArgumentException("Invalid argument combination");
    }
}
