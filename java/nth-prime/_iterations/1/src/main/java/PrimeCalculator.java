import java.util.stream.IntStream;

class PrimeCalculator {
    int nth(int nth) {
        if (nth <= 0) {
            throw new IllegalArgumentException("nth must be greater than or equal to 1");
        }
        return IntStream.iterate(2, i -> i + 1).filter(this::isPrime).skip(nth - 1).findFirst().orElseThrow();
    }

    private boolean isPrime(int number) {
        return IntStream.rangeClosed(2, number / 2).noneMatch(f -> number % f == 0);
    }
}
