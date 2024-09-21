import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

class ParallelLetterFrequency {
    private final String input;

    ParallelLetterFrequency(String input) {
        this.input = input.toLowerCase();
    }

    Map<Integer, Integer> letterCounts() {
        ConcurrentHashMap<Integer, Integer> counts = new ConcurrentHashMap<>();
        input.chars().parallel()
                     .filter(Character::isLetter)
                     .forEach(i -> counts.merge(i, 1, Integer::sum));
        return counts;
    }
}
