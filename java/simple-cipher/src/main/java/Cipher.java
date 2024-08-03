import java.util.Random;

class Cipher {
    private String key;

    Cipher() {
        this(getRandomKey());
    }

    Cipher(String key) {
        this.key = key;
    }

    String getKey() {
        return key;
    }

    String encode(String text) {
        return code(text, 1);
    }

    String decode(String text) {
        return code(text, -1);
    }

    private String code(String text, int direction) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < text.length(); i++) {
            int coded = text.charAt(i) + (key.charAt(i % key.length()) - 'a') * direction;
            if (coded < 'a') {
                coded += ('z' - 'a' + 1);
            } else if (coded > 'z') {
                coded -= ('z' - 'a' + 1);
            }
            sb.append((char) coded);
        }
        return sb.toString();
    }

    private static String getRandomKey() {
        StringBuilder sb = new StringBuilder();
        new Random().ints(100, 'a', 'z' + 1)
                .forEach(c -> sb.append((char) c));
        return sb.toString();
    }
}
