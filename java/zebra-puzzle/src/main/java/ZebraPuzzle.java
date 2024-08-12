import java.util.*;

class ZebraPuzzle {
    private State state;

    ZebraPuzzle() {
        State state = new State();
        List<Statement> statements = getStatements();
        applyStatements(statements, state);
        this.state = guess(statements, state, new LinkedList<>(
                Arrays.asList("owner", "color", "pet", "beverage", "cigarettes")));
    }

    String getWaterDrinker() {
        return getWaterDrinker(state);
    }

    String getZebraOwner() {
        return getZebraOwner(state);
    }

    private static String getWaterDrinker(State state) {
        return getOwnerFor(state, "beverage", "water");
    }

    private static String getZebraOwner(State state) {
        return getOwnerFor(state, "pet", "zebra");
    }

    private static String getOwnerFor(State state, String param, String value) {
        if (state == null) {
            return null;
        }
        House house = state.getWith(param, value);
        if (house != null) {
            return house.get("owner");
        }
        return null;
    }

    private static boolean puzzleIsSolved(List<Statement> statements, State state) {
        return statements.stream().allMatch(st -> st.getCompatibility(state) == Compatibility.PERFECT_MATCH)
                && getWaterDrinker(state) != null && getZebraOwner(state) != null;
    }

    private static State guess(List<Statement> statements, State state, LinkedList<String> params) {
        try {
            String param = params.poll();
            if (param == null) {
                return puzzleIsSolved(statements, state) ? state : null;
            }

            List<List<String>> values = new ArrayList<>();
            for (House house : state.houses) {
                values.add(new ArrayList<>(house.params.get(param)));
            }
            for (int i0 = 0; i0 < values.get(0).size(); i0++) {
                for (int i1 = 0; i1 < values.get(1).size(); i1++) {
                    for (int i2 = 0; i2 < values.get(2).size(); i2++) {
                        for (int i3 = 0; i3 < values.get(3).size(); i3++) {
                            for (int i4 = 0; i4 < values.get(4).size(); i4++) {
                                State clonedState = (State) state.clone();
                                clonedState.set(clonedState.houses.get(0), param, values.get(0).get(i0));
                                clonedState.set(clonedState.houses.get(1), param, values.get(1).get(i1));
                                clonedState.set(clonedState.houses.get(2), param, values.get(2).get(i2));
                                clonedState.set(clonedState.houses.get(3), param, values.get(3).get(i3));
                                clonedState.set(clonedState.houses.get(4), param, values.get(4).get(i4));
                                State finalState = guess(statements, clonedState, new LinkedList<>(params));
                                if (finalState != null) {
                                    return finalState;
                                }
                            }
                        }
                    }
                }
            }
            return null;
        } catch (CloneNotSupportedException cnse) {
            throw new IllegalStateException(cnse);
        }
    }

    private static void applyStatements(List<Statement> statements, State state) {
        while (!statements.isEmpty()) {
            List<Statement> statementsToRemove = new ArrayList<>();
            boolean applied = statements.stream().map(statement -> {
                ApplyResult result = statement.apply(state);
                if (result == ApplyResult.DONE) {
                    statementsToRemove.add(statement);
                }
                return result;
            }).reduce(ApplyResult.NONE, (acc, r) -> r.ordinal() > acc.ordinal() ? r : acc) != ApplyResult.NONE;
            statements.removeAll(statementsToRemove);
            if (!applied) {
                break;
            }
        }
    }

    private static List<Statement> getStatements() {
        return new ArrayList<>(Arrays.asList(
                new LinkStatement("owner", "Englishman", "color", "red"),
                new LinkStatement("owner", "Spaniard", "pet", "dog"),
                new LinkStatement("beverage", "coffee", "color", "green"),
                new LinkStatement("owner", "Ukrainian", "beverage", "tea"),
                new Statement6(),
                new LinkStatement("cigarettes", "Old Gold", "pet", "snails"),
                new LinkStatement("cigarettes", "Kools", "color", "yellow"),
                new AxiomStatement(2, "beverage", "milk"),
                new AxiomStatement(0, "owner", "Norwegian"),
                new ProximityLinkStatement("cigarettes", "Chesterfields", "pet", "fox"),
                new ProximityLinkStatement("cigarettes", "Kools", "pet", "horse"),
                new LinkStatement("cigarettes", "Lucky Strike", "beverage", "orange juice"),
                new LinkStatement("owner", "Japanese", "cigarettes", "Parliaments"),
                new ProximityLinkStatement("owner", "Norwegian", "color", "blue")
        ));
    }
}
