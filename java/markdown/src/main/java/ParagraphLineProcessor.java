class ParagraphLineProcessor extends AbstractLineProcessor {
    @Override
    public boolean apply(String markdown, State state) {
        if (!super.apply(markdown, state)) {
            return false;
        }

        state.htmlBuilder.append("<p>")
                .append(markdown)
                .append("</p>");
        return true;
    }
}
