import java.util.Arrays;
import java.util.Objects;
import java.util.stream.IntStream;

class SumOfMultiples {
    private int sum = 0;

    SumOfMultiples(int number, int[] set) {
        if (Objects.requireNonNull(set).length > 0) {
            this.sum = IntStream.range(1, number)
                                .filter(mult -> Arrays.stream(set).filter(i -> i > 0).anyMatch(i -> mult % i == 0))
                                .sum();
        }
    }

    int getSum() {
        return sum;
    }
}
