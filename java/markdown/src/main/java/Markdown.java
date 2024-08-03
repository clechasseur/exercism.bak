class Markdown {
    String parse(String markdown) {
        State state = new State();
        String[] lines = markdown.split("\n");
        for (String line : lines) {
            String work = line;
            for (ContentProcessor processor : ContentProcessors.getContentProcessors()) {
                work = processor.apply(work);
            }

            for (LineProcessor processor : LineProcessors.getLineProcessors()) {
                if (processor.apply(work, state)) {
                    break;
                }
            }
        }
        while (!state.endTags.isEmpty()) {
            state.htmlBuilder.append(state.endTags.removeLast());
        }
        return state.htmlBuilder.toString();
    }
}