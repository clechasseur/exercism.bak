import java.util.List;
import java.util.Set;

/**
 * <code>Output</code> stores the output of the Hangman game.
 * <p>
 * <i>Note:</i> this class would probably be more efficient as a <code>record</code>,
 * but because it is used as-is by the tests, it cannot be refactored.
 */
class Output {
    public final String secret;
    public final String discovered;
    public final Set<String> guess;
    public final Set<String> misses;
    public final List<Part> parts;
    public final Status status;

    Output(
        final String secret,
        final String discovered,
        final Set<String> guess,
        final Set<String> misses,
        final List<Part> parts,
        final Status status
    ) {
        this.secret = secret;
        this.discovered = discovered;
        this.guess = Set.copyOf(guess);
        this.misses = Set.copyOf(misses);
        this.parts = List.copyOf(parts);
        this.status = status;
    }
}
