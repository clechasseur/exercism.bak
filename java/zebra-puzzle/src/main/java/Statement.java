interface Statement {
    ApplyResult apply(State state);
    Compatibility getCompatibility(State state);
}
