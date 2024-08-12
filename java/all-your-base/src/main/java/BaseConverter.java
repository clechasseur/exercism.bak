import java.util.ArrayList;
import java.util.List;

class BaseConverter {
    private int num;

    BaseConverter(int base, int[] digits) {
        if (base < 2) {
            throw new IllegalArgumentException("Bases must be at least 2.");
        }

        this.num = 0;
        int pow = 1;
        for (int i = digits.length - 1; i >= 0; --i) {
            if (digits[i] < 0) {
                throw new IllegalArgumentException("Digits may not be negative.");
            } else if (digits[i] >= base) {
                throw new IllegalArgumentException("All digits must be strictly less than the base.");
            }
            this.num += digits[i] * pow;
            pow *= base;
        }
    }

    int[] convertToBase(int base) {
        if (base < 2) {
            throw new IllegalArgumentException("Bases must be at least 2.");
        }

        List<Integer> result = new ArrayList<>();
        int num = this.num;
        while (num > 0) {
            result.add(0, num % base);
            num /= base;
        }
        if (result.isEmpty()) {
            result.add(0);
        }
        return result.stream().mapToInt(i -> i).toArray();
    }
}
