import java.util.Arrays;
import java.util.Objects;

class LargestSeriesProductCalculator {
    private final int[] inputDigits;

    LargestSeriesProductCalculator(String inputNumber) {
        if (!Objects.requireNonNull(inputNumber).chars().allMatch(Character::isDigit)) {
            throw new IllegalArgumentException("String to search may only contain digits.");
        }
        this.inputDigits = inputNumber.chars().map(ic -> ic - '0').toArray();
    }

    long calculateLargestProductForSeriesLength(int numberOfDigits) {
        if (numberOfDigits < 0) {
            throw new IllegalArgumentException("Series length must be non-negative.");
        }
        if (numberOfDigits > inputDigits.length) {
            throw new IllegalArgumentException(
                    "Series length must be less than or equal to the length of the string to search.");
        }
        if (numberOfDigits == 0) {
            return 1;
        }

        int largestProduct = 0;
        for (int i = 0; i <= inputDigits.length - numberOfDigits; ++i) {
            int product = Arrays.stream(inputDigits)
                                .skip(i)
                                .limit(numberOfDigits)
                                .reduce(1, (a, b) -> a * b);
            largestProduct = Math.max(product, largestProduct);
        }
        return largestProduct;
    }
}
