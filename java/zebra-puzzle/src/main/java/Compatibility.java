enum Compatibility {
    COMPATIBLE, PERFECT_MATCH, INCOMPATIBLE;

    static Compatibility bestMatch(Compatibility a, Compatibility b) {
        return a.ordinal() > b.ordinal() ? a : b;
    }
}
