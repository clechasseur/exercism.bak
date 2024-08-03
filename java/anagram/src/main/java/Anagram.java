import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

class Anagram {
    private final String sortedWord;

    Anagram(String word) {
        this.sortedWord = preprocess(Objects.requireNonNull(word));
    }

    List<String> match(List<String> candidates) {
        return Objects.requireNonNull(candidates).stream()
                                                 .filter(candidate -> preprocess(candidate).equals(sortedWord))
                                                 .collect(Collectors.toList());
    }

    private String preprocess(String word) {
        // De-capitalize for some reason.
        String cleanedWord = word.substring(0, 1).toLowerCase() + word.substring(1);
        return cleanedWord.chars()
                          .sorted()
                          .mapToObj(i -> (char) i)
                          .map(String::valueOf)
                          .collect(Collectors.joining());
    }
}
