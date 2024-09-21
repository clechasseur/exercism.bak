import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// Port of my Kotlin solution:
// https://exercism.io/tracks/kotlin/exercises/change/solutions/1ee9ddb205b04ab8a442bb38faa5aff6
class ChangeCalculator {
    private List<Integer> coins;

    ChangeCalculator(List<Integer> coins) {
        if (coins.isEmpty()) {
            throw new IllegalArgumentException("Must have at least one coin type");
        } else if (!coins.stream().sorted().collect(Collectors.toList()).equals(coins)) {
            throw new IllegalArgumentException("Coin types must be sorted");
        } else if (coins.stream().anyMatch(c -> c <= 0)) {
            throw new IllegalArgumentException("Coin types must be greater than 0");
        } else if (!coins.stream().distinct().collect(Collectors.toList()).equals(coins)) {
            throw new IllegalArgumentException("Coin types must be unique");
        }
        this.coins = coins;
    }

    List<Integer> computeMostEfficientChange(int amount) {
        List<Integer> change = null;
        if (coins.get(0) == 1) {
            change = possiblyComputeMostEfficientChange(amount,
                    reverse(coins.stream().skip(1).collect(Collectors.toList())));
        }
        if (change == null) {
            change = possiblyComputeMostEfficientChange(amount, reverse(coins));
        }
        if (change == null) {
            throw new IllegalArgumentException("The total " + amount + " cannot be represented in the given currency.");
        }
        return change.stream().sorted().collect(Collectors.toList());
    }

    private List<Integer> possiblyComputeMostEfficientChange(int amount, List<Integer> withCoins) {
        return possiblyComputeMostEfficientChange(amount, withCoins, Integer.MAX_VALUE);
    }

    private List<Integer> possiblyComputeMostEfficientChange(int amount, List<Integer> withCoins, int stepsLeft) {
        if (amount < 0) {
            throw new IllegalArgumentException("Negative totals are not allowed.");
        }
        if (amount == 0) {
            return Collections.emptyList();
        }
        if (stepsLeft == 0) {
            return null;
        }

        List<Integer> change = null;
        for (Integer coin : withCoins) {
            if (amount >= coin) {
                List<Integer> subChange = possiblyComputeMostEfficientChange(amount - coin,
                        withCoins.stream().dropWhile(c -> c > coin).collect(Collectors.toList()),
                        Math.min(stepsLeft - 1, (change != null ? change.size() : Integer.MAX_VALUE) - 1));
                if (subChange != null && (change == null || subChange.size() + 1 < change.size())) {
                    change = Stream.concat(Stream.of(coin), subChange.stream()).collect(Collectors.toList());
                }
            }
        }
        return change;
    }

    private static <T> List<T> reverse(List<T> list) {
        List<T> result = new ArrayList<>();
        for (int i = list.size() - 1; i >= 0; i--) {
            result.add(list.get(i));
        }
        return result;
    }
}
