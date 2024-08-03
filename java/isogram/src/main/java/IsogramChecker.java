import java.util.Objects;

class IsogramChecker {
    boolean isIsogram(String phrase) {
        String cleanedPhrase = Objects.requireNonNull(phrase).toLowerCase()
                                                             .replace(" ", "")
                                                             .replace("-", "");
        return cleanedPhrase.chars().distinct().count() == cleanedPhrase.length();
    }
}
