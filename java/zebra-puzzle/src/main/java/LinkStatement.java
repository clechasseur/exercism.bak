final class LinkStatement implements Statement {
    private final String param1;
    private final String value1;
    private final String param2;
    private final String value2;

    LinkStatement(String param1, String value1, String param2, String value2) {
        this.param1 = param1;
        this.value1 = value1;
        this.param2 = param2;
        this.value2 = value2;
    }

    @Override
    public ApplyResult apply(State state) {
        House house = state.getWith(param1, value1);
        if (house != null) {
            state.set(house, param2, value2);
            return ApplyResult.DONE;
        }

        house = state.getWith(param2, value2);
        if (house != null) {
            state.set(house, param1, value1);
            return ApplyResult.DONE;
        }

        return state.houses.stream().map(h -> {
            ApplyResult res = ApplyResult.NONE;
            if (!h.couldBe(param1, value1) && h.couldBe(param2, value2)) {
                state.remove(h, param2, value2);
                res = ApplyResult.APPLIED;
            }
            if (!h.couldBe(param2, value2) && h.couldBe(param1, value1)) {
                state.remove(h, param1, value1);
                res = ApplyResult.APPLIED;
            }
            return res;
        }).reduce(ApplyResult.NONE, ApplyResult::best);
    }

    @Override
    public Compatibility getCompatibility(State state) {
        return state.houses.stream().map(house -> {
            if (value1.equals(house.get(param1))) {
                if (value2.equals(house.get(param2))) {
                    return Compatibility.PERFECT_MATCH;
                } else if (!house.couldBe(param2, value2)) {
                    return Compatibility.INCOMPATIBLE;
                }
            }
            if (value2.equals(house.get(param2))) {
                if (value1.equals(house.get(param1))) {
                    return Compatibility.PERFECT_MATCH;
                } else if (!house.couldBe(param1, value1)) {
                    return Compatibility.INCOMPATIBLE;
                }
            }
            return Compatibility.COMPATIBLE;
        }).reduce(Compatibility.COMPATIBLE, Compatibility::bestMatch);
    }
}
