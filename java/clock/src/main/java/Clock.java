import java.util.Objects;

class Clock {
    public static final int MINUTES_PER_HOUR = 60;
    public static final int HOURS_PER_DAY = 24;
    public static final int MINUTES_PER_DAY = MINUTES_PER_HOUR * HOURS_PER_DAY;

    private int t;

    Clock(int h, int m) {
        t = (h * MINUTES_PER_HOUR) + m;
        adjust();
    }

    void add(int m) {
        t += m;
        adjust();
    }

    @Override
    public String toString() {
        return String.format("%02d:%02d", t / MINUTES_PER_HOUR, t % MINUTES_PER_HOUR);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Clock clock = (Clock) o;
        return t == clock.t;
    }

    @Override
    public int hashCode() {
        return Objects.hash(t);
    }

    private void adjust() {
        while (t < 0) {
            t += MINUTES_PER_DAY;
        }
        t = t % MINUTES_PER_DAY;
    }
}