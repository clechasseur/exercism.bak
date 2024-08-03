class Scrabble {
    private static final int[] SCORES = new int[] {
         // A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q   R  S  T  U  V  W  X  Y  Z
            1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10
    };

    private final int score;

    Scrabble(String word) {
        this.score = word.toUpperCase().chars().map(c -> SCORES[c - 'A']).sum();
    }

    int getScore() {
        return score;
    }
}
