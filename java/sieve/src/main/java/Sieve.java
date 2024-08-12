import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Sieve {
    private final List<Integer> primes;

    Sieve(int maxPrime) {
        SortedSet<Integer> candidates =
                IntStream.rangeClosed(2, maxPrime).boxed().collect(Collectors.toCollection(TreeSet::new));
        int prime = 2;
        while (prime <= maxPrime) {
            getMultiplesStream(prime, maxPrime).forEach(candidates::remove);
            prime = candidates.tailSet(prime + 1).stream().findFirst().orElse(maxPrime + 1);
        }
        this.primes = new ArrayList<>(candidates);
    }

    List<Integer> getPrimes() {
        return primes;
    }

    private IntStream getMultiplesStream(int number, int max) {
        return IntStream.iterate(number * 2, n -> n <= max, n -> n + number);
    }
}
