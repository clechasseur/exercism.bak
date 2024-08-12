import java.util.stream.Collectors;
import java.util.stream.IntStream;

class FoodChain {
    private static final String[] SWALLOWED = new String[] {
            "fly.",
            "spider.\nIt wriggled and jiggled and tickled inside her.",
            "bird.\nHow absurd to swallow a bird!",
            "cat.\nImagine that, to swallow a cat!",
            "dog.\nWhat a hog, to swallow a dog!",
            "goat.\nJust opened her throat and swallowed a goat!",
            "cow.\nI don't know how she swallowed a cow!",
            "horse.",
    };
    private static final String[] TO_CATCH = new String[] {
            "fly.",
            "spider that wriggled and jiggled and tickled inside her.",
            "bird.",
            "cat.",
            "dog.",
            "goat.",
            "cow.",
    };

    String verse(int num) {
        StringBuilder sb = new StringBuilder("I know an old lady who swallowed a ")
                .append(SWALLOWED[num - 1]).append("\n");
        if (num < 8) {
            for (int i = num - 1; i >= 1; --i) {
                sb.append("She swallowed the ").append(SWALLOWED[i].split("\\.")[0])
                        .append(" to catch the ").append(TO_CATCH[i - 1]).append("\n");
            }
            sb.append("I don't know why she swallowed the ").append(SWALLOWED[0]).append(" Perhaps she'll die.");
        } else {
            sb.append("She's dead, of course!");
        }
        return sb.toString();
    }

    String verses(int start, int endInclusive) {
        return IntStream.rangeClosed(start, endInclusive)
                        .mapToObj(this::verse)
                        .collect(Collectors.joining("\n\n"));
    }
}
