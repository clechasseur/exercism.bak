import java.util.List;
import java.util.stream.Collectors;

class Knapsack {
    int maximumValue(int capacity, List<Item> items) {
        return items.stream().mapToInt(item -> {
            if (item.getWeight() > capacity) {
                return 0;
            }
            return item.getValue() + maximumValue(capacity - item.getWeight(),
                    items.subList(items.indexOf(item) + 1, items.size()));
        }).max().orElse(0);
    }
}
