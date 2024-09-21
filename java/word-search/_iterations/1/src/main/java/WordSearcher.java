import java.util.*;

class WordSearcher {
    private static final List<Pair> DIRECTIONS = Arrays.asList(new Pair(-1, -1), new Pair(0, -1),
            new Pair(1, -1), new Pair(-1, 0), new Pair(1, 0), new Pair(-1, 1),
            new Pair(0, 1), new Pair(1, 1));

    Map<String, Optional<WordLocation>> search(Set<String> searchWords, char[][] puzzle) {
        Map<String, Optional<WordLocation>> output = new HashMap<>();

        for (String word : searchWords) {
            boolean wordFound = false;
            List<Pair> firstLetterLocations = letterLocations(word.charAt(0), puzzle);
            for (int i = 0; !wordFound && i < firstLetterLocations.size(); i++) {
                Pair start = firstLetterLocations.get(i);
                for (int j = 0; !wordFound && j < DIRECTIONS.size(); j++) {
                    Pair direction = DIRECTIONS.get(j);
                    String possibleWord = wordFrom(start, direction, puzzle);
                    if (possibleWord.startsWith(word)) {
                        output.put(word, Optional.of(
                                new WordLocation(start, start.plus(direction.times(word.length() - 1)))));
                        wordFound = true;
                    }
                }
            }
            if (!wordFound) {
                output.put(word, Optional.empty());
            }
        }

        return output;
    }

    private List<Pair> letterLocations(char letter, char[][] puzzle) {
        List<Pair> locations = new ArrayList<>();
        for (int y = 0; y < puzzle.length; y++) {
            for (int x = 0; x < puzzle[y].length; x++) {
                if (puzzle[y][x] == letter) {
                    locations.add(new Pair(x + 1, y + 1));
                }
            }
        }
        return locations;
    }

    private String wordFrom(Pair start, Pair direction, char[][] puzzle) {
        StringBuilder output = new StringBuilder();
        for (Pair pos = start; posValid(pos, puzzle); pos = pos.plus(direction)) {
            output.append(puzzle[pos.getY() - 1][pos.getX() - 1]);
        }
        return output.toString();
    }

    private static boolean posValid(Pair pos, char[][] puzzle) {
        return pos.getY() >= 1 && pos.getY() <= puzzle.length
                && pos.getX() >= 1 && pos.getX() <= puzzle[pos.getY() - 1].length;
    }
}