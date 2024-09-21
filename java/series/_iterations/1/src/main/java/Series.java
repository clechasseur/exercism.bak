import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Series {
    private final String input;

    public Series(String input) {
        this.input = Objects.requireNonNull(input);
    }

    List<String> slices(int size) {
        if (size <= 0) {
            throw new IllegalArgumentException("Slice size is too small.");
        } else if (size > input.length()) {
            throw new IllegalArgumentException("Slice size is too big.");
        }

        return IntStream.rangeClosed(0, input.length() - size)
                        .mapToObj(i -> input.substring(i, i + size))
                        .collect(Collectors.toList());
    }
}
