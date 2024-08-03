import java.util.Objects;

public class PangramChecker {
    public boolean isPangram(String input) {
        return Objects.requireNonNull(input).replaceAll("[^a-zA-Z]", "")
                                            .toLowerCase()
                                            .chars()
                                            .distinct()
                                            .count() == 26;
    }
}
