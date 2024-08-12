import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class PigLatinTranslator {
    private static final Pattern WORDS = Pattern.compile("\\w+");
    private static final Pattern VOWELS_PREFIX = Pattern.compile("([aeiou]|yt|xr)\\w*");
    private static final Pattern CONSONANTS_PREFIX = Pattern.compile("(qu|[^aeiouq]+qu|[^aeiou]+)([aeiouy]\\w*)");

    public String translate(String input) {
        return WORDS.matcher(Objects.requireNonNull(input)).replaceAll(mr -> translateWord(mr.group()));
    }

    private String translateWord(String word) {
        if (VOWELS_PREFIX.matcher(word).matches()) {
            return word + "ay";
        } else {
            Matcher matcher = CONSONANTS_PREFIX.matcher(word);
            if (matcher.matches()) {
                return matcher.group(2) + matcher.group(1) + "ay";
            } else {
                throw new IllegalStateException("Word cannot be translated: " + word);
            }
        }
    }
}
