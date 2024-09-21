import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

final class PythagoreanTriplet {
    private final int a;
    private final int b;
    private final int c;

    PythagoreanTriplet(int a, int b, int c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    int getA() {
        return a;
    }

    int getB() {
        return b;
    }

    int getC() {
        return c;
    }

    static ListBuilder makeTripletsList() {
        return new ListBuilder();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PythagoreanTriplet that = (PythagoreanTriplet) o;
        return getA() == that.getA() &&
                getB() == that.getB() &&
                getC() == that.getC();
    }

    @Override
    public int hashCode() {
        return Objects.hash(getA(), getB(), getC());
    }

    static final class ListBuilder {
        private int factorsLessThanOrEqualTo;
        private int sumTo;

        ListBuilder withFactorsLessThanOrEqualTo(int factorsLessThanOrEqualTo) {
            this.factorsLessThanOrEqualTo = factorsLessThanOrEqualTo;
            return this;
        }

        ListBuilder thatSumTo(int sumTo) {
            this.sumTo = sumTo;
            return this;
        }

        List<PythagoreanTriplet> build() {
            List<PythagoreanTriplet> result = new ArrayList<>();
            for (int a = 1; a <= factorsLessThanOrEqualTo - 2 && (a * 3 + 3) <= sumTo; a++) {
                for (int b = a + 1; b <= factorsLessThanOrEqualTo - 1 && (a + b * 2 + 1) <= sumTo; b++) {
                    final int c = sumTo - (a + b);
                    if (c <= factorsLessThanOrEqualTo && (a * a) + (b * b) == (c * c)) {
                        result.add(new PythagoreanTriplet(a, b, c));
                    }
                }
            }
            return result;
        }
    }
}