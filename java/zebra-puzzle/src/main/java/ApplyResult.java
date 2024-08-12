enum ApplyResult {
    NONE, APPLIED, DONE;

    static ApplyResult best(ApplyResult a, ApplyResult b) {
        return a.ordinal() > b.ordinal() ? a : b;
    }
}
