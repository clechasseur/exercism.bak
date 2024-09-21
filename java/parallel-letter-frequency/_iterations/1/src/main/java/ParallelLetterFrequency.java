import java.util.HashMap;
import java.util.Map;

class ParallelLetterFrequency {
    private final String input;

    ParallelLetterFrequency(String input) {
        this.input = input.toLowerCase();
    }

    Map<Integer, Integer> letterCounts() {
        Map<Integer, Integer> counts = new HashMap<>();
        for (int i = 0; i < input.length(); ++i) {
            char c = input.charAt(i);
            if (c >= 'a' && c <= 'z') {
                counts.merge((int) c, 1, Integer::sum);
            }
        }
        return counts;
    }
}
