import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Acronym {
    private static final Pattern WORD_ACRONYM = Pattern.compile("([A-Z])[A-Z']*");

    private final String acronym;

    Acronym(String phrase) {
        String cleanedPhrase = Objects.requireNonNull(phrase).toUpperCase().replaceAll("[^A-Z']", " ");
        Matcher matcher = WORD_ACRONYM.matcher(cleanedPhrase);
        this.acronym = matcher.replaceAll(mr -> mr.group(1)).replace(" ", "");
    }

    String get() {
        return acronym;
    }
}
