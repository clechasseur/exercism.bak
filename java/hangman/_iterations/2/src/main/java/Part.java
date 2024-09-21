import java.util.Arrays;
import java.util.List;

/**
 * <code>Part</code> represents each part of the hanged man that appear in the
 * game when a wrong guess is made. After 6 wrong guesses, the game is lost.
 */
enum Part {
    HEAD,
    BODY,
    LEFT_ARM,
    RIGHT_ARM,
    LEFT_LEG,
    RIGHT_LEG;

    /**
     * A list containing all hangman parts.
     */
    public static final List<Part> PARTS = Arrays.stream(Part.values()).toList();
}
