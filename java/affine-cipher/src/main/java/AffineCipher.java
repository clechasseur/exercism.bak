import java.util.stream.Collectors;

class AffineCipher {
    String encode(String input, int a, int b) {
        return input.replaceAll("[ .,]", "")
                    .toLowerCase()
                    .chars()
                    .mapToObj(c -> encodeOne(c, a, b))
                    .collect(Collectors.joining())
                    .replaceAll(".{1,5}", "$0 ")
                    .trim();
    }

    String decode(String input, int a, int b) {
        return input.replaceAll(" ", "")
                    .toLowerCase()
                    .chars()
                    .mapToObj(c -> decodeOne(c, a, b))
                    .collect(Collectors.joining());
    }

    private String encodeOne(int c, int a, int b) {
        calculateMMI(a);

        if (c >= 'a' && c <= 'z') {
            int x = c - 'a';
            int ex = (a * x + b) % 26;
            int newC = ex + 'a';
            return Character.toString((char) newC);
        }

        return Character.toString((char) c);
    }

    private String decodeOne(int c, int a, int b) {
        int mmi = calculateMMI(a);

        if (c >= 'a' && c <= 'z') {
            int y = c - 'a';
            int dy = (mmi * (y - b)) % 26;
            if (dy < 0) {
                dy += 26;
            }
            int newC = dy + 'a';
            return Character.toString((char) newC);
        }

        return Character.toString((char) c);
    }

    // Taken from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
    private int calculateMMI(int a) {
        int t = 0, newt = 1, r = 26, newr = a;
        while (newr != 0) {
            int quotient = r / newr;
            int oldt = t;
            t = newt;
            newt = oldt - quotient * newt;
            int oldr = r;
            r = newr;
            newr = oldr - quotient * newr;
        }
        if (r > 1) {
            throw new IllegalArgumentException("Error: keyA and alphabet size must be coprime.");
        }
        if (t < 0) {
            t += 26;
        }
        return t;
    }
}
