import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public class Robot {
    private static final Random RANDOM = new Random();
    private static final Set<String> NAMES = new HashSet<>();

    private String name;

    public String getName() {
        if (name == null) {
            name = generateNewName();
        }
        return name;
    }

    public void reset() {
        name = null;
    }

    private static String generateNewName() {
        String name = null;
        while (name == null) {
            name = String.format("%c%c%d%d%d", randomChar(), randomChar(), randomDigit(), randomDigit(), randomDigit());
            if (NAMES.contains(name)) {
                name = null;
            }
        }
        NAMES.add(name);
        return name;
    }

    private static char randomChar() {
        return (char) (((int) 'A') + RANDOM.nextInt(26));
    }

    private static int randomDigit() {
        return RANDOM.nextInt(10);
    }
}
