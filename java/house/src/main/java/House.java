import java.util.stream.Collectors;
import java.util.stream.IntStream;

class House {
    private static final String[] SUBJECTS = new String[] {
            "house that Jack built",
            "malt",
            "rat",
            "cat",
            "dog",
            "cow with the crumpled horn",
            "maiden all forlorn",
            "man all tattered and torn",
            "priest all shaven and shorn",
            "rooster that crowed in the morn",
            "farmer sowing his corn",
            "horse and the hound and the horn",
    };
    private static final String[] ACTIONS = new String[] {
            "lay in",
            "ate",
            "killed",
            "worried",
            "tossed",
            "milked",
            "kissed",
            "married",
            "woke",
            "kept",
            "belonged to",
    };

    String verse(int num) {
        StringBuilder sb = new StringBuilder("This is the ").append(SUBJECTS[num - 1]);
        for (int i = num - 2; i >= 0; --i) {
            sb.append(" that ").append(ACTIONS[i]).append(" the ").append(SUBJECTS[i]);
        }
        return sb.append(".").toString();
    }

    String verses(int start, int endInclusive) {
        return IntStream.rangeClosed(start, endInclusive)
                        .mapToObj(this::verse)
                        .collect(Collectors.joining("\n"));
    }

    String sing() {
        return verses(1, 12);
    }
}
