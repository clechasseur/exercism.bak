import java.util.Comparator;
import java.util.Map;
import java.util.TreeMap;

class ResistorColor {
    private static final Map<String, Integer> BANDS = new TreeMap<>() {{
        put("black", 0);
        put("brown", 1);
        put("red", 2);
        put("orange", 3);
        put("yellow", 4);
        put("green", 5);
        put("blue", 6);
        put("violet", 7);
        put("grey", 8);
        put("white", 9);
    }};

    int colorCode(String color) {
        if (!BANDS.containsKey(color)) {
            throw new IllegalArgumentException("unknown color.");
        }
        return BANDS.get(color);
    }

    String[] colors() {
        return BANDS.entrySet().stream()
                               .sorted(Comparator.comparingInt(Map.Entry::getValue))
                               .map(Map.Entry::getKey)
                               .toArray(String[]::new);
    }
}
