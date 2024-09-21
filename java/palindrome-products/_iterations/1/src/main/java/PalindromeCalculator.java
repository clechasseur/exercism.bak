import java.util.*;

class PalindromeCalculator {
    SortedMap<Long, List<List<Integer>>> getPalindromeProductsWithFactors(int beginInclusive, int endInclusive) {
        if (endInclusive < beginInclusive) {
            throw new IllegalArgumentException("invalid input: min must be <= max");
        }

        SortedMap<Long, List<List<Integer>>> palindromes = new TreeMap<>();
        for (int i = beginInclusive; i <= endInclusive; i++) {
            for (int j = i; j <= endInclusive; j++) {
                final long product = (long) i * (long) j;
                if (isPalindrome(product)) {
                    palindromes.computeIfAbsent(product, p -> new ArrayList<>()).add(Arrays.asList(i, j));
                }
            }
        }
        return palindromes;
    }

    private static boolean isPalindrome(long n) {
        String s = String.valueOf(n);
        for (int i = 0, j = s.length() - 1; i <= s.length() / 2; i++, j--) {
            if (s.charAt(i) != s.charAt(j)) {
                return false;
            }
        }
        return true;
    }
}
