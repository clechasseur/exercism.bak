import java.util.Arrays;
import java.util.List;

class ResistorColorDuo {
    private static final List<String> COLORS = Arrays.asList("black", "brown", "red", "orange", "yellow",
            "green", "blue", "violet", "grey", "white");

    int value(String[] colors) {
        return COLORS.indexOf(colors[0]) * 10 + COLORS.indexOf(colors[1]);
    }
}
