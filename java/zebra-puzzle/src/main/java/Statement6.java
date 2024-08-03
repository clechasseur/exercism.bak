final class Statement6 implements Statement {
    @Override
    public ApplyResult apply(State state) {
        // "The green house is immediately to the right of the ivory house."

        House house = state.getWith("color", "green");
        if (house != null) {
            int houseIdx = state.houses.indexOf(house);
            state.set(state.houses.get(houseIdx - 1), "color", "ivory");
            return ApplyResult.DONE;
        }

        house = state.getWith("color", "ivory");
        if (house != null) {
            int houseIdx = state.houses.indexOf(house);
            state.set(state.houses.get(houseIdx + 1), "color", "green");
            return ApplyResult.DONE;
        }

        ApplyResult result = ApplyResult.NONE;
        if (state.houses.get(0).couldBe("color", "green")) {
            state.remove(state.houses.get(0), "color", "green");
            result = ApplyResult.APPLIED;
        }
        if (state.houses.get(4).couldBe("color", "ivory")) {
            state.remove(state.houses.get(4), "color", "ivory");
            result = ApplyResult.APPLIED;
        }
        result = state.houses.stream().map(h ->  {
            ApplyResult res = ApplyResult.NONE;
            if (!h.couldBe("color", "green")) {
                int hIdx = state.houses.indexOf(h);
                if (hIdx > 0 && state.houses.get(hIdx - 1).couldBe("color", "ivory")) {
                    state.remove(state.houses.get(hIdx - 1), "color", "ivory");
                    res = ApplyResult.APPLIED;
                }
            }
            if (!h.couldBe("color", "ivory")) {
                int hIdx = state.houses.indexOf(h);
                if (hIdx < 4 && state.houses.get(hIdx + 1).couldBe("color", "green")) {
                    state.remove(state.houses.get(hIdx + 1), "color", "green");
                    res = ApplyResult.APPLIED;
                }
            }
            return res;
        }).reduce(result, ApplyResult::best);

        if (!state.houses.get(1).couldBe("color", "green")
                && !state.houses.get(1).couldBe("color", "ivory")
                && state.houses.get(3).couldBeOtherThan("color", "green", "ivory")) {
            state.houses.get(3).set("color", "green", "ivory");
            result = ApplyResult.APPLIED;
        }
        if (!state.houses.get(3).couldBe("color", "green")
                && !state.houses.get(3).couldBe("color", "ivory")
                && state.houses.get(1).couldBeOtherThan("color", "green", "ivory")) {
            state.houses.get(1).set("color", "green", "ivory");
            result = ApplyResult.APPLIED;
        }

        return result;
    }

    @Override
    public Compatibility getCompatibility(State state) {
        Compatibility compatibility = Compatibility.COMPATIBLE;
        for (int houseIdx = 0; houseIdx < state.houses.size(); houseIdx++) {
            House house = state.houses.get(houseIdx);
            House toTheLeft = houseIdx > 0 ? state.houses.get(houseIdx - 1) : null;
            House toTheRight = houseIdx < 4 ? state.houses.get(houseIdx + 1) : null;
            if ("green".equals(house.get("color"))) {
                if (toTheLeft != null && "ivory".equals(toTheLeft.get("color"))) {
                    compatibility = Compatibility.PERFECT_MATCH;
                } else if (toTheLeft == null || !toTheLeft.couldBe("color", "ivory")) {
                    return Compatibility.INCOMPATIBLE;
                }
            }
            if ("ivory".equals(house.get("color"))) {
                if (toTheRight != null && "green".equals(toTheRight.get("color"))) {
                    compatibility = Compatibility.PERFECT_MATCH;
                } else if (toTheRight == null || !toTheRight.couldBe("color", "green")) {
                    return Compatibility.INCOMPATIBLE;
                }
            }
        }
        return compatibility;
    }
}
