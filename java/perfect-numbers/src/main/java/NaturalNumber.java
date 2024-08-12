import java.util.stream.IntStream;

class NaturalNumber {
    private final int number;

    NaturalNumber(int number) {
        if (number <= 0) {
            throw new IllegalArgumentException("You must supply a natural number (positive integer)");
        }
        this.number = number;
    }

    Classification getClassification() {
        int aliquot = getAliquotSum();
        if (aliquot < number) {
            return Classification.DEFICIENT;
        } else if (aliquot > number) {
            return Classification.ABUNDANT;
        } else {
            return Classification.PERFECT;
        }
    }

    private int getAliquotSum() {
        return IntStream.rangeClosed(1, number / 2).filter(i -> number % i == 0).sum();
    }
}
