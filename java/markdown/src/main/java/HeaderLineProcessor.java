import java.util.regex.Matcher;
import java.util.regex.Pattern;

class HeaderLineProcessor extends AbstractLineProcessor {
    private static final Pattern PATTERN = Pattern.compile("^(#+) (.*)$");

    @Override
    public boolean apply(String markdown, State state) {
        Matcher matcher = PATTERN.matcher(markdown);
        if (!matcher.matches() || !super.apply(markdown, state)) {
            return false;
        }

        state.htmlBuilder.append("<h")
                .append(matcher.group(1).length())
                .append('>')
                .append(matcher.group(2))
                .append("</h")
                .append(matcher.group(1).length())
                .append('>');
        return true;
    }
}
