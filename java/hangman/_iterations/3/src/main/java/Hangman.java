import io.reactivex.Observable;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

import static java.util.Collections.emptySet;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toUnmodifiableSet;

class Hangman {
    /**
     * Given two <code>Observable</code>s providing the words for new games and the
     * letters guessed, returns a new <code>Observable</code> that returns the game
     * state for each operation in the form of an {@link Output}.
     * <p>
     * <i>Note:</i> this method is stateless, e.g. game state is computed solely from
     * the data provided by the two input <code>Observable</code>s and is not stored
     * in this class. Thus, it is possible to play more than one game at a time.
     *
     * @param words   <code>Observable</code> for new games; must provide the secret words
     * @param letters <code>Observable</code> for guesses; must provide the letters guessed (as strings)
     * @return        <code>Observable</code> for game state changes
     */
    Observable<Output> play(Observable<String> words, Observable<String> letters) {
        return words
                .map(word -> (Function<State, State>) _state -> State.forNewGame(word))
                .mergeWith(letters.map(letter -> state -> state.withGuess(letter)))
                .scan(State.empty(), (state, op) -> op.apply(state))
                .skip(1) // The first state is empty, skip it
                .map(State::toOutput);
    }

    /**
     * A list containing all hangman {@link Part}s.
     * <p>
     * <i>Note:</i> in theory, this should probably be stored in the <code>Part</code> enum.
     * However, it seems that submitting the <code>Part.java</code> file with the exercise
     * doesn't work on the Exercism platform; thus, I'll store this list here.
     */
    private static final List<Part> PARTS = Arrays.stream(Part.values()).toList();

    /**
     * <code>State</code> represents the state of a hangman game. It is kept
     * separate from the {@link Output} class because it is optimized for
     * storage and easy mutation by {@link Operation operations}.
     * <p>
     * Can be converted to an <code>Output</code> via {@link #toOutput()}.
     *
     * @param secret  the secret word
     * @param guesses all letters that have been guessed so far (whether hits or misses)
     * @param hits    number of letters discovered in the secret word
     * @param misses  number of wrong guesses
     */
    private record State(String secret, Set<Character> guesses, int hits, int misses) {
        /**
         * Returns an empty <code>State</code>, with an empty secret word.
         *
         * @return empty state
         */
        static State empty() {
            return new State("", emptySet(), 0, 0);
        }

        /**
         * Returns a <code>State</code> for a new game with the specific secret word.
         *
         * @param secret secret word
         * @return       state for new game
         */
        static State forNewGame(String secret) {
            return new State(secret, emptySet(), 0, 0);
        }

        /**
         * Attempts to guess a letter of the secret word and returns a new <code>State</code>
         * with that guess. The original <code>State</code> is not modified.
         *
         * @param guess letter guessed
         * @return      new game state
         *
         * @throws IllegalStateException    if game is already completed
         * @throws IllegalArgumentException if <code>guess</code> is empty, contains more than
         *                                  one letter or was already guessed before
         */
        State withGuess(String guess) {
            if (getStatus() != Status.PLAYING) {
                throw new IllegalStateException("Cannot guess letter when game is completed");
            }
            if (guess.length() != 1) {
                throw new IllegalArgumentException("Guess must be single character, got \"" + guess + "\"");
            }

            var singleGuess = guess.charAt(0);
            if (guesses.contains(singleGuess)) {
                throw new IllegalArgumentException("Letter " + guess + " was already played");
            }

            var newHits = hitsFor(singleGuess);
            return new State(
                    secret,
                    Stream.concat(guesses.stream(), Stream.of(singleGuess)).collect(toUnmodifiableSet()),
                    hits + newHits,
                    misses + (newHits == 0 ? 1 : 0)
            );
        }

        /**
         * Converts this <code>State</code> to an {@link Output}.
         *
         * @return <code>Output</code> corresponding to this <code>State</code>
         */
        Output toOutput() {
            return new Output(
                    secret,
                    getDiscovered(),
                    getGuess(),
                    getMisses(),
                    getParts(),
                    getStatus()
            );
        }

        /**
         * Returns the number of occurrences of the given letter in the secret word.
         * If the guess is wrong, returns 0.
         *
         * @param guess letter guessed
         * @return      hits for the letter guessed (0 for a miss)
         */
        private int hitsFor(char guess) {
            return (int) secret.codePoints().filter(c -> (char) c == guess).count();
        }

        /**
         * Returns the {@link #guesses() guesses} as a <code>Stream</code> of strings.
         *
         * @return guesses stream
         */
        private Stream<String> guessesAsString() {
            return guesses.stream().map(c -> Character.toString(c));
        }

        /**
         * Returns the character to display in the discovered string for a specific letter
         * (expressed as a code point). If the letter is in the secret word, it will be returned
         * (as a string); otherwise, <code>_</code> (an underscore) is returned.
         *
         * @param codePoint letter guessed
         * @return          character to display for <code>codePoint</code> (as a string)
         */
        private String discoveredCharFor(int codePoint) {
            return guesses.contains((char) codePoint) ? Character.toString(codePoint) : "_";
        }

        /**
         * Returns a string representation of the discovered letters in the hangman game.
         * Letters guessed correctly will be displayed while letters not yet guessed will
         * be displayed as <code>_</code> (underscores).
         * <p>
         * This can be used as {@link Output#discovered}.
         *
         * @return string containing discovered letters
         */
        private String getDiscovered() {
            return secret.codePoints().mapToObj(this::discoveredCharFor).collect(joining());
        }

        /**
         * Returns the set of correct guesses.
         * <p>
         * This can be used as {@link Output#guess}.
         *
         * @return set of letters guessed correctly
         */
        private Set<String> getGuess() {
            return guessesAsString().filter(secret::contains).collect(toUnmodifiableSet());
        }

        /**
         * Returns the set of incorrect guesses.
         * <p>
         * This can be used as {@link Output#misses}.
         *
         * @return set of letters guessed incorrectly
         */
        private Set<String> getMisses() {
            return guessesAsString().filter(not(secret::contains)).collect(toUnmodifiableSet());
        }

        /**
         * Returns the list of hangman <code>Part</code>s to display. One
         * hangman part must be displayed for every miss.
         * <p>
         * This can be used as {@link Output#parts}.
         *
         * @return list of hangman parts to display
         */
        private List<Part> getParts() {
            return PARTS.stream().limit(misses).toList();
        }

        /**
         * Returns the game's current {@link Status}.
         * <p>
         * This can be used as {@link Output#status}.
         *
         * @return game status
         */
        private Status getStatus() {
            if (misses == PARTS.size()) {
                return Status.LOSS;
            } else if (hits == secret.length()) {
                return Status.WIN;
            } else {
                return Status.PLAYING;
            }
        }
    }
}
