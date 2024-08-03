import java.util.Objects;

final class ProximityLinkStatement implements Statement {
    private final String param1;
    private final String value1;
    private final String param2;
    private final String value2;

    ProximityLinkStatement(String param1, String value1, String param2, String value2) {
        this.param1 = param1;
        this.value1 = value1;
        this.param2 = param2;
        this.value2 = value2;
    }

    @Override
    public ApplyResult apply(State state) {
        ApplyResult result = applyOneSided(state, param1, value1, param2, value2);
        if (result == ApplyResult.DONE) {
            return ApplyResult.DONE;
        }

        return ApplyResult.best(result, applyOneSided(state, param2, value2, param1, value1));
    }

    private ApplyResult applyOneSided(State state, String p1, String v1, String p2, String v2) {
        ApplyResult result = ApplyResult.NONE;
        House house = state.getWith(p1, v1);
        if (house != null) {
            int houseIdx = state.houses.indexOf(house);
            House toTheLeft = houseIdx > 0 ? state.houses.get(houseIdx - 1) : null;
            House toTheRight = houseIdx < 4 ? state.houses.get(houseIdx + 1) : null;
            if ((toTheLeft != null && Objects.equals(toTheLeft.get(p2), v2)) ||
                    (toTheRight != null && Objects.equals(toTheRight.get(p2), v2))) {
                return ApplyResult.DONE;
            }
            if (toTheLeft != null && toTheLeft.get(p2) != null) {
                toTheLeft = null;
            }
            if (toTheRight != null && toTheRight.get(p2) != null) {
                toTheRight = null;
            }
            if (toTheLeft != null || toTheRight != null) {
                if (toTheLeft == null) {
                    state.set(toTheRight, p2, v2);
                    return ApplyResult.DONE;
                } else if (toTheRight == null) {
                    state.set(toTheLeft, p2, v2);
                    return ApplyResult.DONE;
                } else {
                    final House house1 = toTheLeft;
                    final House house2 = toTheRight;
                    state.houses.stream()
                            .filter(h -> h != house1 && h != house2)
                            .forEach(h -> state.remove(h, p2, v2));
                    result = ApplyResult.APPLIED;
                }
            }
        }

        return state.houses.stream().map(h -> {
            ApplyResult res = ApplyResult.NONE;
            if (!h.couldBe(p1, v1)) {
                int hIdx = state.houses.indexOf(h);
                House toTheLeft = hIdx > 0 ? state.houses.get(hIdx - 1) : null;
                House toTheRight = hIdx < 4 ? state.houses.get(hIdx + 1) : null;
                if (toTheLeft != null && toTheLeft.couldBe(p2, v2)) {
                    state.remove(toTheLeft, p2, v2);
                    res = ApplyResult.APPLIED;
                }
                if (toTheRight != null && toTheRight.couldBe(p2, v2)) {
                    state.remove(toTheRight, p2, v2);
                    res = ApplyResult.APPLIED;
                }
            }
            return res;
        }).reduce(result, ApplyResult::best);
    }

    @Override
    public Compatibility getCompatibility(State state) {
        Compatibility compatibility = Compatibility.COMPATIBLE;
        for (int houseIdx = 0; houseIdx < state.houses.size(); houseIdx++) {
            House house = state.houses.get(houseIdx);
            House toTheLeft = houseIdx > 0 ? state.houses.get(houseIdx - 1) : null;
            House toTheRight = houseIdx < 4 ? state.houses.get(houseIdx + 1) : null;
            if (value1.equals(house.get(param1))) {
                if (houseIs(toTheLeft, param2, value2) || houseIs(toTheRight, param2, value2)) {
                    compatibility = Compatibility.PERFECT_MATCH;
                } else if (houseCouldntBe(toTheLeft, param2, value2) && houseCouldntBe(toTheRight, param2, value2)) {
                    return Compatibility.INCOMPATIBLE;
                }
            }
            if (value2.equals(house.get(param2))) {
                if (houseIs(toTheLeft, param1, value1) || houseIs(toTheRight, param1, value1)) {
                    compatibility = Compatibility.PERFECT_MATCH;
                } else if (houseCouldntBe(toTheLeft, param1, value1) && houseCouldntBe(toTheRight, param1, value1)) {
                    return Compatibility.INCOMPATIBLE;
                }
            }
        }
        return compatibility;
    }

    private static boolean houseIs(House house, String param, String value) {
        return house != null && value.equals(house.get(param));
    }

    private static boolean houseCouldntBe(House house, String param, String value) {
        return house == null || !house.couldBe(param, value);
    }
}
