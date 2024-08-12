final class AxiomStatement implements Statement {
    private final int houseIdx;
    private final String param;
    private final String value;

    AxiomStatement(int houseIdx, String param, String value) {
        this.houseIdx = houseIdx;
        this.param = param;
        this.value = value;
    }

    @Override
    public ApplyResult apply(State state) {
        state.set(state.houses.get(houseIdx), param, value);
        return ApplyResult.DONE;
    }

    @Override
    public Compatibility getCompatibility(State state) {
        return value.equals(state.houses.get(houseIdx).get(param))
                ? Compatibility.PERFECT_MATCH
                : Compatibility.INCOMPATIBLE;
    }
}
