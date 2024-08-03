import java.util.Objects;
import java.util.stream.Collectors;

class RotationalCipher {
    private final int shiftKey;

    RotationalCipher(int shiftKey) {
        this.shiftKey = shiftKey;
    }

    String rotate(String data) {
        return Objects.requireNonNull(data).chars()
                                           .map(this::rotate)
                                           .mapToObj(c -> String.valueOf((char) c))
                                           .collect(Collectors.joining());
    }

    private int rotate(int letter) {
        if (letter >= 'a' && letter <= 'z') {
            return 'a' + ((letter - 'a') + shiftKey) % 26;
        } else if (letter >= 'A' && letter <= 'Z') {
            return 'A' + ((letter - 'A') + shiftKey) % 26;
        } else {
            return letter;
        }
    }
}
