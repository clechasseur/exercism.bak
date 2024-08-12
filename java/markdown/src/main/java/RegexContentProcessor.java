import java.util.regex.Pattern;

class RegexContentProcessor implements ContentProcessor {
    private final Pattern pattern;
    private final String replacement;

    RegexContentProcessor(final String pattern, final String replacement) {
        this.pattern = Pattern.compile(pattern);
        this.replacement = replacement;
    }

    @Override
    public String apply(String markdown) {
        return pattern.matcher(markdown).replaceAll(replacement);
    }
}
