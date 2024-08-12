import java.util.*;

class BookStore {
    double calculateBasketCost(List<Integer> basket) {
        return calculateBasketCost(
                new ArrayList<>(Collections.singletonList(new ArrayList<>())),
                convertBasket(basket)
        );
    }

    private Map<Integer, Integer> convertBasket(List<Integer> basket) {
        Map<Integer, Integer> newBasket = new TreeMap<>();
        for (Integer book : basket) {
            newBasket.put(book, newBasket.getOrDefault(book, 0) + 1);
        }
        return newBasket;
    }

    private double calculateBasketCost(List<List<Integer>> groups, Map<Integer, Integer> basket) {
        if (basket.isEmpty()) {
            return calculateFinalCost(groups);
        }

        List<Integer> lastGroup = groups.get(groups.size() - 1);
        int minBook = lastGroup.isEmpty() ? 1 : lastGroup.get(lastGroup.size() - 1) + 1;
        OptionalDouble minCost = OptionalDouble.empty();
        for (int book = minBook; book <= 5; book++) {
            Integer remaining = basket.get(book);
            if (remaining != null) {
                Map<Integer, Integer> newBasket = new TreeMap<>(basket);
                if (remaining > 1) {
                    newBasket.put(book, remaining - 1);
                } else {
                    newBasket.remove(book);
                }
                int lastGroupIdx = groups.size() - 1;
                List<List<Integer>> newGroups = new ArrayList<>(groups);
                newGroups.set(lastGroupIdx, new ArrayList<>(newGroups.get(lastGroupIdx)));
                newGroups.get(lastGroupIdx).add(book);
                double thisCost = calculateBasketCost(newGroups, newBasket);
                if (minCost.isEmpty() || minCost.getAsDouble() > thisCost) {
                    minCost = OptionalDouble.of(thisCost);
                }
            }
        }
        if (lastGroup.size() > 3
                || (lastGroup.size() == 2 && basket.size() <= 2)
                || (lastGroup.size() == 3 && basket.size() <= 3)) {
            List<List<Integer>> newGroups = new ArrayList<>(groups);
            newGroups.add(new ArrayList<>());
            double thisCost = calculateBasketCost(newGroups, basket);
            if (minCost.isEmpty() || minCost.getAsDouble() > thisCost) {
                minCost = OptionalDouble.of(thisCost);
            }
        }
        if (minCost.isEmpty()) {
            List<List<Integer>> newGroups = new ArrayList<>(groups);
            for (int book = 1; book <= 5; book++) {
                Integer numBooks = basket.get(book);
                if (numBooks != null) {
                    for (int i = 0; i < numBooks; i++) {
                        newGroups.add(Collections.singletonList(book));
                    }
                }
            }
            minCost = OptionalDouble.of(calculateFinalCost(newGroups));
        }
        return minCost.getAsDouble();
    }

    private double calculateFinalCost(List<List<Integer>> groups) {
        return groups.stream().mapToDouble(group -> GROUP_COSTS.get(group.size())).sum();
    }

    private static final Map<Integer, Double> GROUP_COSTS = new TreeMap<>() {{
        put(0, 0.0);
        put(1, 8.0);
        put(2, (8.0 * 2) * 0.95);
        put(3, (8.0 * 3) * 0.9);
        put(4, (8.0 * 4) * 0.8);
        put(5, (8.0 * 5) * 0.75);
    }};
}
