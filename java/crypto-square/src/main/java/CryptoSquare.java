import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

class CryptoSquare {
    private static final Pattern NON_WORD = Pattern.compile("\\W");

    private final String ciphertext;

    CryptoSquare(String phrase) {
        String normalized = NON_WORD.matcher(phrase).replaceAll("").toLowerCase();
        int columns = (int) Math.ceil(Math.sqrt(normalized.length()));
        List<String> square = new ArrayList<>();
        for (int i = 0; i < normalized.length(); i += columns) {
            square.add(normalized.substring(i, Math.min(i + columns, normalized.length())));
        }
        StringBuilder ciphertextBuilder = new StringBuilder();
        for (int i = 0; i < columns; i++) {
            if (i != 0) {
                ciphertextBuilder.append(' ');
            }
            for (String line : square) {
                ciphertextBuilder.append(i < line.length() ? line.charAt(i) : ' ');
            }
        }
        ciphertext = ciphertextBuilder.toString();
    }

    String getCiphertext() {
        return ciphertext;
    }
}
