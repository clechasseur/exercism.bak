import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.IntStream;

class ParallelLetterFrequency {
    private final String[] input;

    ParallelLetterFrequency(String[] input) {
        this.input = input;
    }

    Map<Character, Integer> countLetters() {
        var counts = new ConcurrentHashMap<Character, Integer>();

        var codePoints = IntStream.empty();
        for (var s : input) {
            codePoints = IntStream.concat(codePoints, s.codePoints());
        }
        
        codePoints.parallel()
                  .map(Character::toLowerCase)
                  .filter(Character::isLetter)
                  .forEach(i -> {
                      for (var c : Character.toString(i).toCharArray()) {
                          counts.merge(c, 1, Integer::sum);
                      } 
                  });

        return counts;
    }
}
