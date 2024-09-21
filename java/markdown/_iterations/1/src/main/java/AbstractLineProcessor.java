import java.util.Objects;

abstract class AbstractLineProcessor implements LineProcessor {
    @Override
    public boolean apply(String markdown, State state) {
        String thisEndTag = endTag();
        String curEndTag = state.endTags.peekLast();
        if ((thisEndTag == null || !Objects.equals(thisEndTag, curEndTag)) && curEndTag != null) {
            state.htmlBuilder.append(curEndTag);
            state.endTags.removeLast();
        }
        return true;
    }

    @Override
    public String endTag() {
        return null;
    }
}
