interface LineProcessor {
    boolean apply(String markdown, State state);
    String endTag();
}
