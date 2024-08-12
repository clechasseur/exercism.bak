enum MeetupSchedule {
    FIRST,
    SECOND,
    THIRD,
    FOURTH,
    LAST,
    TEENTH;

    private static final MeetupSchedule[] VALUES = values();

    MeetupSchedule previous() {
        return this != FIRST ? VALUES[ordinal() - 1] : TEENTH;
    }
}
