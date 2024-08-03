import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Yacht {
    private static final int[] LITTLE_STRAIGHT = { 1, 2, 3, 4, 5 };
    private static final int[] BIG_STRAIGHT = { 2, 3, 4, 5, 6 };
    private static final Map<YachtCategory, Function<IntStream, Integer>> CATEGORY_CALCS = new HashMap<>() {{
        put(YachtCategory.YACHT, dice -> dice.distinct().count() == 1 ? 50 : 0);
        put(YachtCategory.ONES, dice -> (int) dice.filter(d -> d == 1).count());
        put(YachtCategory.TWOS, dice -> (int) dice.filter(d -> d == 2).count() * 2);
        put(YachtCategory.THREES, dice -> (int) dice.filter(d -> d == 3).count() * 3);
        put(YachtCategory.FOURS, dice -> (int) dice.filter(d -> d == 4).count() * 4);
        put(YachtCategory.FIVES, dice -> (int) dice.filter(d -> d == 5).count() * 5);
        put(YachtCategory.SIXES, dice -> (int) dice.filter(d -> d == 6).count() * 6);
        put(YachtCategory.FULL_HOUSE, dice -> {
            Map<Integer, Integer> dicePerCount = getDicePerCount(dice);
            if (dicePerCount.size() == 2 && dicePerCount.containsKey(3)) {
                return dicePerCount.entrySet().stream().mapToInt(e -> e.getKey() * e.getValue()).sum();
            } else {
                return 0;
            }
        });
        put(YachtCategory.FOUR_OF_A_KIND, dice -> {
            Map<Integer, Integer> dicePerCount = getDicePerCount(dice);
            int howManyOf = dicePerCount.containsKey(4) ? 4 : (dicePerCount.containsKey(5) ? 5 : 0);
            return howManyOf != 0 ? dicePerCount.get(howManyOf) * 4 : 0;
        });
        put(YachtCategory.LITTLE_STRAIGHT, dice -> Arrays.equals(dice.toArray(), LITTLE_STRAIGHT) ? 30 : 0);
        put(YachtCategory.BIG_STRAIGHT, dice -> Arrays.equals(dice.toArray(), BIG_STRAIGHT) ? 30 : 0);
        put(YachtCategory.CHOICE, IntStream::sum);
    }};

    private final int yachtScore;

    Yacht(int[] dice, YachtCategory yachtCategory) {
        this.yachtScore = CATEGORY_CALCS.get(yachtCategory).apply(Arrays.stream(dice).sorted());
    }

    int score() {
        return yachtScore;
    }

    private static Map<Integer, Integer> getDicePerCount(IntStream dice) {
        // This doesn't work for duplicate counts, but we're not using it for that, so we're OK.
        return dice.boxed()
                   .collect(Collectors.toMap(d -> d, d -> 1, Integer::sum))
                   .entrySet().stream()
                   .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey, (a, b) -> a));
    }
}
