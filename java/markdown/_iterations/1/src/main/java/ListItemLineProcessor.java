import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class ListItemLineProcessor extends AbstractLineProcessor {
    private static final Pattern PATTERN = Pattern.compile("^\\* (.*)$");

    @Override
    public boolean apply(String markdown, State state) {
        Matcher matcher = PATTERN.matcher(markdown);
        if (!matcher.matches() || !super.apply(markdown, state)) {
            return false;
        }

        String curEndTag = state.endTags.peekLast();
        if (!Objects.equals("</ul>", curEndTag)) {
            state.htmlBuilder.append("<ul>");
            state.endTags.addLast("</ul>");
        }
        state.htmlBuilder.append("<li>")
                .append(matcher.group(1))
                .append("</li>");
        return true;
    }

    @Override
    public String endTag() {
        return "</ul>";
    }
}
