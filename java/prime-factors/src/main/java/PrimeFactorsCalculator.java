import java.util.ArrayList;
import java.util.List;

public class PrimeFactorsCalculator {
    public List<Long> calculatePrimeFactorsOf(long number) {
        List<Long> factors = new ArrayList<>();
        long factor = 2;
        while (number > 1) {
            if (number % factor == 0) {
                factors.add(factor);
                number /= factor;
            } else {
                ++factor;
            }
        }
        return factors;
    }
}
