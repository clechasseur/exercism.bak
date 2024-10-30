import java.util.List;

class Knapsack {
    int maximumValue(int capacity, List<Item> items) {
        return items
                .stream()
                .mapToInt(item -> {
                    if (item.weight > capacity) {
                        return 0;
                    }

                    return item.value + maximumValue(
                            capacity - item.weight,
                            items.subList(items.indexOf(item) + 1, items.size())
                    );
                })
                .max()
                .orElse(0);
    }
}
