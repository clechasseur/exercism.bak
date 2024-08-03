import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

class LuhnValidator {
    private static final Pattern LUHN_PATTERN = Pattern.compile("[0-9]{2,}");

    boolean isValid(String candidate) {
        if (candidate == null) {
            return false;
        }
        String cleanedCandidate = candidate.replace(" ", "");
        if (!LUHN_PATTERN.matcher(cleanedCandidate).matches()) {
            return false;
        }

        List<Integer> digits = new ArrayList<>();
        cleanedCandidate.chars().forEach(dc -> digits.add(dc - '0'));
        for (int i = digits.size() - 2; i >= 0; i -= 2) {
            int doubled = digits.get(i) * 2;
            if (doubled > 9) {
                doubled -= 9;
            }
            digits.set(i, doubled);
        }
        return digits.stream().mapToInt(i -> i).sum() % 10 == 0;
    }
}
